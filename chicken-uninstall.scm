;;;; chicken-uninstall.scm
;
; Copyright (c) 2008-2015, The CHICKEN Team
; All rights reserved.
;
; Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following
; conditions are met:
;
;   Redistributions of source code must retain the above copyright notice, this list of conditions and the following
;     disclaimer. 
;   Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following
;     disclaimer in the documentation and/or other materials provided with the distribution. 
;   Neither the name of the author nor the names of its contributors may be used to endorse or promote
;     products derived from this software without specific prior written permission. 
;
; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS
; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
; AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR
; CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
; OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
; POSSIBILITY OF SUCH DAMAGE.

(module main ()

  (import scheme chicken)
  (import setup-api)
  (import chicken.data-structures
	  chicken.files
	  chicken.foreign
	  chicken.format
	  chicken.irregex
	  chicken.ports
	  chicken.posix)

  (include "mini-srfi-1.scm")

  (define-foreign-variable C_TARGET_LIB_HOME c-string)
  (define-foreign-variable C_BINARY_VERSION int)

  (define *cross-chicken* (feature? #:cross-chicken))
  (define *host-extensions* *cross-chicken*)
  (define *target-extensions* *cross-chicken*)
  (define *prefix* #f)
  (define *deploy* #f)

  (define (repo-path)
    (if *deploy*
	*prefix*
	(if (and *cross-chicken* (not *host-extensions*))
	    (make-pathname C_TARGET_LIB_HOME (sprintf "chicken/~a" C_BINARY_VERSION))
	    (if *prefix*
		(make-pathname
		 *prefix*
		 (sprintf "lib/chicken/~a" (##sys#fudge 42)))
		(repository-path)))))

  (define *force* #f)

  (define (grep rx lst)
    (filter (cut irregex-search rx <>) lst))

  (define (gather-eggs patterns)
    (let* ((eggs (map pathname-file 
		      (glob (make-pathname (repo-path) "*" "setup-info"))))
	   (pats (concatenate (map (cut grep <> eggs) patterns))))
      (delete-duplicates pats)))

  (define (fini code)
    (print "aborted.")
    (exit code))

  (define (ask eggs)
    (handle-exceptions ex
	(if (eq? ex 'aborted)
	    (fini 1) 
	    (signal ex))
      (yes-or-no? 
       (string-intersperse
	(append
	 '("About to delete the following extensions:\n\n")
	 (map (cut string-append "  " <> "\n") eggs)
	 '("\nDo you want to proceed?"))
	"")
       default: "no"
       abort: (abort-setup))))

  (define (uninstall pats)
    (let ((eggs (gather-eggs pats)))
      (cond ((null? eggs)
	     (print "nothing to remove.") )
	    ((or *force* (ask eggs))
	     (for-each
	      (lambda (e)
		(print "removing " e)
		(cond ((and *host-extensions* *target-extensions*)
		       (remove-extension e)
		       (fluid-let ((*host-extensions* #f))
			 (remove-extension e (repo-path)) ))
		      (else (remove-extension e (repo-path)))))
	      eggs)))))

  (define (usage code)
    (print #<<EOF
usage: chicken-uninstall [OPTION | PATTERN] ...

  -h   -help                    show this message and exit
       -version                 show version and exit
       -force                   don't ask, delete whatever matches
       -exact                   treat PATTERN as exact match (not a pattern)
  -s   -sudo                    use sudo(1) for deleting files
  -p   -prefix PREFIX           change installation prefix to PREFIX
       -deploy                  prefix is a deployment directory
       -host                    when cross-compiling, uninstall host extensions only
       -target                  when cross-compiling, uninstall target extensions only
EOF
);| (sic)
    (exit code))

  (define *short-options* '(#\h #\s #\p))

  (define (main args)
    (let ((exact #f))
      (let loop ((args args) (pats '()))
	(cond ((null? args)
	       (when (null? pats) (usage 1))
	       (when (and *deploy* (not *prefix*))
		 (with-output-to-port (current-error-port)
		   (cut print "`-deploy' only makes sense in combination with `-prefix DIRECTORY`"))
		 (exit 1))
	       (uninstall
		(reverse
		 (map
		  (lambda (p)
		    (if exact
			(irregex (string-append "^" (irregex-quote p) "$"))
			(##sys#glob->regexp p)))
		  pats))))
	      (else
	       (let ((arg (car args)))
		 (cond ((or (string=? arg "-help") 
			    (string=? arg "-h")
			    (string=? arg "--help"))
			(usage 0))
		       ((string=? arg "-version")
			(print (chicken-version))
			(exit 0))
		       ((string=? arg "-target")
			(set! *host-extensions* #f)
			(loop (cdr args) pats))
		       ((string=? arg "-host")
			(set! *target-extensions* #f)
			(loop (cdr args) pats))
		       ((string=? arg "-force")
			(set! *force* #t)
			(loop (cdr args) pats))
		       ((string=? arg "-exact")
			(set! exact #t)
			(loop (cdr args) pats))
		       ((or (string=? arg "-s") (string=? arg "-sudo"))
			(sudo-install #t)
			(loop (cdr args) pats))
		       ((string=? "-deploy" arg)
			(set! *deploy* #t)
			(loop (cdr args) pats))
		       ((or (string=? arg "-p") (string=? arg "-prefix"))
			(unless (pair? (cdr args)) (usage 1))
			(set! *prefix*
			  (let ((p (cadr args)))
			    (if (absolute-pathname? p)
				p
				(normalize-pathname
				 (make-pathname (current-directory) p) ) ) ) )
			(loop (cddr args) pats))
		       ((and (positive? (string-length arg))
			     (char=? #\- (string-ref arg 0)))
			(if (> (string-length arg) 2)
			    (let ((sos (string->list (substring arg 1))))
			      (if (every (cut memq <> *short-options*) sos)
				  (loop
				   (append (map (cut string #\- <>) sos) (cdr args)) pats)
				  (usage 1)))
			    (usage 1)))
		       (else (loop (cdr args) (cons arg pats))))))))))

  (main (command-line-arguments))
  
 )
