;;;; chicken-uninstall.scm
;
; Copyright (c) 2008-2016, The CHICKEN Team
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

  (import (scheme)
          (chicken))
  (import (chicken data-structures)
	  (chicken files)
          (chicken foreign)
          (chicken io)
	  (chicken format)
	  (chicken irregex)
	  (chicken ports)
	  (chicken posix))

(include "mini-srfi-1.scm")
(include "egg-environment.scm")
(include "egg-information.scm")

(define host-extensions #t)
(define target-extensions #t)
(define force-uninstall #f)
(define sudo-uninstall #f)

(define (repo-path)
  (destination-repository
    (if (and cross-chicken (not host-extensions))
        'target
        'host)))

(define (grep rx lst)
  (filter (cut irregex-search rx <>) lst))

(define (gather-eggs patterns)
  (let* ((eggs (map pathname-file 
                 (glob (make-pathname (repo-path) "*" +egg-info-extension+))))
         (pats (concatenate (map (cut grep <> eggs) patterns))))
    (delete-duplicates pats)))

(define (fini code)
  (print "aborted.")
  (exit code))

(define (ask eggs)
  (print* (string-intersperse
           (append '("About to delete the following extensions:\n\n")
                   (map (cut string-append "  " <> "\n") eggs)
                   '("\nDo you want to proceed ? (no/yes) "))
           ""))
  (flush-output)
  (let loop ()
    (let ((r (trim (read-line))))
      (cond ((string=? r "yes"))
            ((string=? r "no") (fini 1))
            (else (loop))))))

(define (trim str)
  (define (left lst)
    (cond ((null? lst) '())
          ((char-whitespace? (car lst)) (left (cdr lst)))
          (else (cons (car lst) (left (cdr lst))))))
  (list->string (reverse (left (reverse (left (string->list str)))))))
 
(define (remove-extension egg #!optional (repo (repo-path)))
  (and-let* ((ifile (make-pathname repo egg +egg-info-extension+))
             (files (get-egg-property* (load-egg-info ifile) 'installed-files)))
    (for-each
      (lambda (f)
        (let ((p (if (absolute-pathname? f) f (make-pathname repo f))))
          (when (file-exists? p) (delete-installed-file p))))
      (cdr files))
    (delete-installed-file ifile)))

(define (delete-file-command platform)
  (case platform
    ((unix) "rm -f ")
    ((windows) "del /q /s ")))

(define (delete-installed-file fname)
  (cond ((not (file-exists? fname))
         (warning "file does not exist" fname))
        ((and sudo-uninstall (eq? 'unix default-platform))
         (let ((r (system (string-append "sudo " (delete-file-command 'unix) 
                                         "\"" fname "\""))))
           (unless (zero? r)
             (warning "deleting file failed" fname))))
        (else (delete-file fname))))

(define (uninstall pats)
  (let ((eggs (gather-eggs pats)))
    (cond ((null? eggs)
           (print "nothing to remove.") )
          ((or force-uninstall (ask eggs))
           (for-each
             (lambda (e)
               (print "removing " e)
               (when host-extensions (remove-extension e))
               (when (and cross-chicken target-extensions)
                 (remove-extension e (destination-repository 'target))))
             eggs)))))

(define (usage code)
  (print #<<EOF
usage: chicken-uninstall [OPTION | PATTERN] ...

  -h   -help                    show this message and exit
       -version                 show version and exit
       -force                   don't ask, delete whatever matches
       -exact                   treat PATTERN as exact match (not a pattern)
  -s   -sudo                    use external command to elevate privileges for deleting files
       -host                    when cross-compiling, uninstall host extensions only
       -target                  when cross-compiling, uninstall target extensions only
EOF
)
  (exit code))

(define short-options '(#\h #\s #\p))

(define (main args)
  (let ((exact #f))
    (let loop ((args args) (pats '()))
      (cond ((null? args)
             (when (null? pats) (usage 1))
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
                       (set! host-extensions #f)
                       (loop (cdr args) pats))
                      ((string=? arg "-host")
                       (set! target-extensions #f)
                       (loop (cdr args) pats))
                      ((string=? arg "-force")
                       (set! force-uninstall #t)
                       (loop (cdr args) pats))
                      ((string=? arg "-exact")
                       (set! exact #t)
                       (loop (cdr args) pats))
                      ((or (string=? arg "-s") (string=? arg "-sudo"))
                       (set! sudo-uninstall #t)
                       (loop (cdr args) pats))
                      ((and (positive? (string-length arg))
                            (char=? #\- (string-ref arg 0)))
                       (if (> (string-length arg) 2)
                           (let ((sos (string->list (substring arg 1))))
                             (if (every (cut memq <> short-options) sos)
                                 (loop (append (map (cut string #\- <>) sos)
                                               (cdr args)) pats)
                                 (usage 1)))
                           (usage 1)))
                      (else (loop (cdr args) (cons arg pats))))))))))

(main (command-line-arguments))
  
)
