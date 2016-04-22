;;;; files.scm - File operations
;
; Copyright (c) 2008-2015, The CHICKEN Team
; Copyright (c) 2000-2007, Felix L. Winkelmann
; All rights reserved.
;
; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions
; are met:
;
;   Redistributions of source code must retain the above copyright
;   notice, this list of conditions and the following disclaimer.
;
;   Redistributions in binary form must reproduce the above copyright
;   notice, this list of conditions and the following disclaimer in
;   the documentation and/or other materials provided with the
;   distribution.
;
;   Neither the name of the author nor the names of its contributors
;     may be used to endorse or promote products derived from this
;     software without specific prior written permission.
;
; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
; FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
; COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
; INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
; STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
; OF THE POSSIBILITY OF SUCH DAMAGE.


(declare
  (unit files)
  (uses extras pathname)
  (fixnum)
  (disable-interrupts) 
  (foreign-declare #<<EOF
#include <errno.h>

#ifndef _WIN32
# include <sys/stat.h>
# define C_mkdir(str)       C_fix(mkdir(C_c_string(str), S_IRWXU | S_IRWXG | S_IRWXO))
#else
# define C_mkdir(str)	    C_fix(mkdir(C_c_string(str)))
#endif
EOF
))

(module chicken.files
  (delete-file* file-copy file-move
   create-temporary-directory
   create-temporary-file)

(import scheme chicken)
(import chicken.foreign
	chicken.io
	chicken.pathname)

(include "common-declarations.scm")


(define-foreign-variable strerror c-string "strerror(errno)")


;;; Like `delete-file', but does nothing if the file doesn't exist:

(define delete-file*
  (lambda (file)
    (and (file-exists? file) (delete-file file)) ) )

;;; file-copy and file-move : they do what you'd think.

(define (file-copy origfile newfile #!optional (clobber #f) (blocksize 1024))
  (##sys#check-string origfile 'file-copy)
  (##sys#check-string newfile 'file-copy)
  (##sys#check-number blocksize 'file-copy)
  (unless (and (integer? blocksize) (> blocksize 0))
    (##sys#error 
     'file-copy 
     "invalid blocksize given: not a positive integer"
     blocksize))
  (and (file-exists? newfile)
       (or clobber
	   (##sys#error 
	    'file-copy
	    "newfile exists but clobber is false"
	     newfile)))
  (when (directory-exists? origfile)
    (##sys#error 'file-copy "can not copy directories" origfile))
  (let* ((i (open-input-file origfile #:binary))
	 (o (open-output-file newfile #:binary))
	 (s (make-string blocksize)))
    (let loop ((d (read-string! blocksize s i))
	       (l 0))
      (if (fx= 0 d)
	  (begin
	    (close-input-port i)
	    (close-output-port o)
	    l)
	  (begin
	    (write-string s d o)
	    (loop (read-string! blocksize s i) (fx+ d l)))))))

(define (file-move origfile newfile #!optional (clobber #f) (blocksize 1024))
  (##sys#check-string origfile 'file-move)
  (##sys#check-string newfile 'file-move)
  (##sys#check-number blocksize 'file-move)
  (unless (and (integer? blocksize) (> blocksize 0))
    (##sys#error
     'file-move
     "invalid blocksize given: not a positive integer"
     blocksize))
  (when (directory-exists? origfile)
    (##sys#error 'file-move "can not move directories" origfile))
  (and (file-exists? newfile)
       (or clobber
	   (##sys#error 
	    'file-move
	    "newfile exists but clobber is false"
	    newfile)))
  (let* ((i (open-input-file origfile #:binary))
	 (o (open-output-file newfile #:binary))
	 (s (make-string blocksize)))
    (let loop ((d (read-string! blocksize s i))
	       (l 0))
      (if (fx= 0 d)
	  (begin
	    (close-input-port i)
	    (close-output-port o)
	    (delete-file origfile)
	    l)
	  (begin
	    (write-string s d o)
	    (loop (read-string! blocksize s i) (fx+ d l)))))))


;;; Temporary file creation:

(define create-temporary-file)
(define create-temporary-directory)

(let ((temp #f)
      (temp-prefix "temp")
      (string-append string-append))
  (define (tempdir)
    (or temp
	(let ((tmp 
	       (or (get-environment-variable "TMPDIR") 
		   (get-environment-variable "TEMP")
		   (get-environment-variable "TMP")
		   "/tmp")))
	  (set! temp tmp)
	  tmp)))
  (set! create-temporary-file
    (lambda (#!optional (ext "tmp"))
      (##sys#check-string ext 'create-temporary-file)
      (let loop ()
	(let* ((n (##core#inline "C_random_fixnum" #x10000))
	       (pn (make-pathname 
		    (tempdir)
		    (string-append 
		     temp-prefix
		     (number->string n 16)
		     "."
		     (##sys#number->string (##sys#fudge 33))) ; PID
		    ext)) )
	  (if (file-exists? pn)
	      (loop)
	      (call-with-output-file pn (lambda (p) pn)) ) ) ) ) )
  (set! create-temporary-directory
    (lambda ()
      (let loop ()
	(let* ((n (##core#inline "C_random_fixnum" #x10000))
	       (pn (make-pathname 
		    (tempdir)
		    (string-append
		     temp-prefix
		     (number->string n 16)
		     "."
		     (##sys#number->string (##sys#fudge 33)))))) ; PID
	  (if (directory-exists? pn) 
	      (loop)
	      (let ((r (##core#inline "C_mkdir" (##sys#make-c-string pn 'create-temporary-directory))))
		(if (eq? r 0)
		    pn
		    (##sys#signal-hook 
		     #:file-error 'create-temporary-directory
		     (##sys#string-append "cannot create temporary directory - " strerror)
		     pn))))))))))
