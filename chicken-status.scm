;;;; chicken-status.scm
;
; Copyright (c) 2008-2017, The CHICKEN Team
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

  (import (scheme))
  (import (chicken))
  (import (chicken data-structures)
	  (chicken file)
	  (chicken foreign)
	  (chicken format)
	  (chicken irregex)
	  (chicken port)
	  (chicken posix)
          (chicken pathname)
	  (chicken pretty-print))

  (include "mini-srfi-1.scm")
  (include "egg-environment.scm")
  (include "egg-information.scm")

  (define host-extensions #t)
  (define target-extensions #t)

  (define (repo-path)
    (if (and cross-chicken (not host-extensions))
        (destination-repository 'target)
        (repository-path)))

  (define (grep rx lst)
    (filter (cut irregex-search rx <>) lst))

  (define (read-info egg)
    (load-egg-info
     (or (chicken.load#find-file
          (make-pathname #f egg +egg-info-extension+)
          (repo-path))
         (error "egg not found" egg))))

  (define (filter-eggs patterns mtch)
    (let* ((eggs (gather-eggs))
           (names (cond ((null? patterns) eggs)
                        (mtch
                         (concatenate
                           (map (lambda (pat)
                                  (grep (irregex (glob->sre pat)) eggs))
                             patterns)))
                        (else 
                          (filter 
                            (lambda (egg)
                              (any (cut string=? <> egg) patterns))
                            eggs)))))
      (delete-duplicates names string=?)))

  (define (gather-eggs)
    (delete-duplicates
      (append-map
        (lambda (dir)
          (map pathname-file 
            (glob (make-pathname dir "*" +egg-info-extension+))))
        (##sys#split-path (repo-path)))
      equal?))

  (define (format-string str cols #!optional right (padc #\space))
    (let* ((len (string-length str))
	   (pad (make-string (fxmax 0 (fx- cols len)) padc)) )
      (if right
	  (string-append pad str)
	  (string-append str pad) ) ) )

  (define get-terminal-width
    (let ((default-width 80))	     ; Standard default terminal width
      (lambda ()
	(let ((cop (current-output-port)))
	  (if (terminal-port? cop)
	      (let ((w (nth-value 1 (terminal-size cop))))
		(if (zero? w) 
		    default-width 
		    (min default-width w)))
	      default-width)))))

  (define (list-installed-eggs eggs)
    (let ((w (quotient (- (get-terminal-width) 2) 2)))
      (for-each
       (lambda (egg)
	 (let ((version (get-egg-property (read-info egg) 'version)))
	   (if version
	       (print
		(format-string (string-append egg " ") w #f #\.)
		(format-string 
		 (string-append " version: " (->string version))
		 w #t #\.))
	       (print egg))))
       (sort eggs string<?))))

  (define (gather-components lst mode)
    (append-map (cut gather-components-rec <> mode) lst))

  (define (gather-components-rec info mode)
    (case (car info)
      ((host) 
       (if host-extensions (gather-components (cdr info) 'host) '()))
      ((target) 
       (if target-extensions (gather-components (cdr info) 'target) '()))
      ((extension) (list (list 'extension mode (cadr info))))
      ((data) (list (list 'data mode (cadr info))))
      ((generated-source-file) (list (list 'generated-source-file mode (cadr info))))
      ((c-include) (list (list 'c-include mode (cadr info))))
      ((scheme-include) (list (list 'scheme-include mode (cadr info))))
      ((program) (list (list 'program mode (cadr info))))))

  (define (list-installed-components eggs)
    (let ((w (quotient (- (get-terminal-width) 2) 2)))
      (for-each
        (lambda (egg)
          (let* ((info (read-info egg))
                 (version (get-egg-property info 'version))
                 (comps (get-egg-property* info 'components)))
            (if version
                (print (format-string (string-append egg " ") w #f #\.)
                       (format-string (string-append " version: "
                                                     (->string version))
                                      w #t #\.))
                (print egg))
            (when comps
              (let ((lst (gather-components comps #f)))
                (for-each
                  (lambda (comp)
                    (print "  " (format-string (->string (car comp)) 32)
                           "  " (format-string (->string (caddr comp)) 32)
                           (case (cadr comp)
                             ((host) " (host)")
                             ((target) " (target)")
                             (else ""))))
                  lst)))))
        eggs)))

  (define (list-installed-files eggs)
    (for-each
     print
     (sort
      (append-map
       (lambda (egg)
	 (get-egg-property* (read-info egg) 'installed-files))
       eggs)
      string<?)))

  (define (dump-installed-versions)
    (for-each
     (lambda (egg)
       (let ((version (get-egg-property (read-info egg) 'version)))
	 (pp (list (string->symbol egg) (or version "???")))))
     (gather-eggs)))

  (define (usage code)
    (print #<<EOF
usage: chicken-status [OPTION ...] [NAME ...]

  -h   -help                    show this message
       -version                 show version and exit
  -a   -all                     scan all repositories in CHICKEN_REPOSITORY_PATH
  -c   -components              list installed components
  -f   -files                   list installed files
       -list                    dump installed extensions and their versions in "override" format
       -match                   treat NAME as glob pattern
       -host                    when cross-compiling, only show host extensions
       -target                  when cross-compiling, only show target extensions
EOF
);|
    (exit code))

  (define short-options '(#\h #\f #\c #\a))

  (define (main args)
    (let ((files #f)
          (comps #f)
          (dump #f)
          (mtch #f))
      (let loop ((args args) (pats '()))
        (if (null? args)
            (cond ((and comps (or dump files))
                   (with-output-to-port (current-error-port)
                     (cut print "-components cannot be used with -list."))
                   (exit 1))
                  (dump (dump-installed-versions))
                  (else
                    (let ((eggs (filter-eggs pats mtch)))
                      (if (null? eggs)
                          (display "(none)\n" (current-error-port))
                          ((cond (comps list-installed-components)
                                 (files list-installed-files)
                                 (else list-installed-eggs))
                           eggs)))))
            (let ((arg (car args)))
              (cond ((member arg '("-help" "-h" "--help"))
                     (usage 0))
		    ((string=? arg "-host")
		     (set! target-extensions #f)
		     (loop (cdr args) pats))
		    ((string=? arg "-target")
		     (set! host-extensions #f)
		     (loop (cdr args) pats))
                    ((string=? arg "-match")
                     (set! mtch #t)
                     (loop (cdr args) pats))
		    ((string=? arg "-list")
		     (set! dump #t)
		     (loop (cdr args) pats))
		    ((or (string=? arg "-f") (string=? arg "-files"))
		     (set! files #t)
		     (loop (cdr args) pats))
		    ((or (string=? arg "-c") (string=? arg "-components"))
		     (set! comps #t)
		     (loop (cdr args) pats))
		    ((string=? arg "-version")
		     (print (chicken-version))
		     (exit 0))
                    ((and (positive? (string-length arg))
                          (char=? #\- (string-ref arg 0)))
                     (if (> (string-length arg) 2)
                         (let ((sos (string->list (substring arg 1))))
                           (if (every (cut memq <> short-options) sos)
                               (loop (append (map (cut string #\- <>) sos)
                                             (cdr args)) pats)
                               (usage 1)))
                         (usage 1)))
                    (else (loop (cdr args) (cons arg pats)))))))))

  (main (command-line-arguments))
  
 )
