;;;; file.scm - File operations
;
; Copyright (c) 2008-2017, The CHICKEN Team
; Copyright (c) 2000-2007, Felix L. Winkelmann
; All rights reserved.
;
; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions are
; met:
;
;   Redistributions of source code must retain the above copyright
;   notice, this list of conditions and the following disclaimer.
;
;   Redistributions in binary form must reproduce the above copyright
;   notice, this list of conditions and the following disclaimer in the
;   documentation and/or other materials provided with the distribution.
;
;   Neither the name of the author nor the names of its contributors may
;   be used to endorse or promote products derived from this software
;   without specific prior written permission.
;
; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
; HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
; BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
; OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR
; TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
; USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
; DAMAGE.


(declare
  (unit file)
  (uses extras irregex pathname posix)
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

#define C_rmdir(str)	    C_fix(rmdir(C_c_string(str)))
EOF
))

(module chicken.file
  (block-device?
   character-device?
   create-directory
   create-fifo
   create-symbolic-link
   create-temporary-directory
   create-temporary-file
   delete-directory
   delete-file
   delete-file*
   directory
   directory-exists?
   directory?
   fifo?
   file-copy
   file-execute-access?
   file-exists?
   file-move
   file-read-access?
   file-type
   file-write-access?
   find-files
   glob
   read-symbolic-link
   regular-file?
   rename-file
   socket?
   symbolic-link?)

(import chicken scheme
	chicken.foreign
	chicken.io
	chicken.irregex
	chicken.pathname
	chicken.posix)

(include "common-declarations.scm")

(define-foreign-variable strerror c-string "strerror(errno)")

;; TODO: Some duplication from POSIX, to give better error messages.
;; This really isn't so much posix-specific, and code like this is
;; also in library.scm.  This should be deduplicated across the board.
(define posix-error
  (let ([strerror (foreign-lambda c-string "strerror" int)]
	[string-append string-append] )
    (lambda (type loc msg . args)
      (let ([rn (##sys#update-errno)])
	(apply ##sys#signal-hook type loc (string-append msg " - " (strerror rn)) args) ) ) ) )


;;; Like `delete-file', but does nothing if the file doesn't exist:

(define delete-file*
  (lambda (file)
    (and (file-exists? file) (delete-file file))))


;;; Directory management:

(define delete-directory
  (lambda (name #!optional recursive)
    (define (rmdir dir)
      (let ((sname (##sys#make-c-string dir)))
	(unless (fx= 0 (##core#inline "C_rmdir" sname))
	  (posix-error #:file-error 'delete-directory "cannot delete directory" dir))))
    (##sys#check-string name 'delete-directory)
    (if recursive
	(let ((files (find-files ; relies on `find-files' to list dir-contents before dir
		      name
		      dotfiles: #t
		      follow-symlinks: #f)))
	  (for-each
	   (lambda (f)
	     ((cond ((symbolic-link? f) delete-file)
		    ((directory? f) rmdir)
		    (else delete-file))
	      f))
	   files)
	  (rmdir name))
	(rmdir name))))

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
	       (getpid (foreign-lambda int "C_getpid"))
	       (pn (make-pathname
		    (tempdir)
		    (string-append
		     temp-prefix
		     (number->string n 16)
		     "."
		     (##sys#number->string (getpid)))
		    ext)))
	  (if (file-exists? pn)
	      (loop)
	      (call-with-output-file pn (lambda (p) pn)))))))
  (set! create-temporary-directory
    (lambda ()
      (let loop ()
	(let* ((n (##core#inline "C_random_fixnum" #x10000))
	       (getpid (foreign-lambda int "C_getpid"))
	       (pn (make-pathname
		    (tempdir)
		    (string-append
		     temp-prefix
		     (number->string n 16)
		     "."
		     (##sys#number->string (getpid))))))
	  (if (file-exists? pn)
	      (loop)
	      (let ((r (##core#inline "C_mkdir" (##sys#make-c-string pn 'create-temporary-directory))))
		(if (eq? r 0)
		    pn
		    (##sys#signal-hook
		     #:file-error 'create-temporary-directory
		     (##sys#string-append "cannot create temporary directory - " strerror)
		     pn)))))))))


;;; Filename globbing:

(define glob
  (lambda paths
    (let conc-loop ((paths paths))
      (if (null? paths)
	  '()
	  (let ((path (car paths)))
	    (let-values (((dir fil ext) (decompose-pathname path)))
	      (let ((rx (irregex (glob->sre (make-pathname #f (or fil "*") ext)))))
		(let loop ((fns (directory (or dir ".") #t)))
		  (cond ((null? fns) (conc-loop (cdr paths)))
			((irregex-match rx (car fns)) =>
			 (lambda (m)
			   (cons (make-pathname dir (irregex-match-substring m))
				 (loop (cdr fns)))))
			(else (loop (cdr fns))))))))))))


;;; Find matching files:

(define (find-files dir #!key (test (lambda _ #t))
			      (action (lambda (x y) (cons x y)))
			      (seed '())
			      (limit #f)
			      (dotfiles #f)
			      (follow-symlinks #f))
  (##sys#check-string dir 'find-files)
  (let* ((depth 0)
	 (lproc
	  (cond ((not limit) (lambda _ #t))
		((fixnum? limit) (lambda _ (fx< depth limit)))
		(else limit)))
	 (pproc
	  (if (procedure? test)
	      test
	      (let ((test (irregex test))) ; force compilation
		(lambda (x) (irregex-match test x))))))
    (let loop ((dir dir)
	       (fs (directory dir dotfiles))
	       (r seed))
      (if (null? fs)
	  r
	  (let* ((filename (##sys#slot fs 0))
		 (f (make-pathname dir filename))
		 (rest (##sys#slot fs 1)))
	    (cond ((directory? f)
		   (cond ((member filename '("." "..")) (loop dir rest r))
			 ((and (symbolic-link? f) (not follow-symlinks))
			  (loop dir rest (if (pproc f) (action f r) r)))
			 ((lproc f)
			  (loop dir
				rest
				(fluid-let ((depth (fx+ depth 1)))
				  (loop f
					(directory f dotfiles)
					(if (pproc f) (action f r) r)))))
			 (else (loop dir rest (if (pproc f) (action f r) r)))))
		  ((pproc f) (loop dir rest (action f r)))
		  (else (loop dir rest r))))))))

)
