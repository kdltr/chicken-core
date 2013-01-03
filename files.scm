;;;; files.scm - File and pathname operations
;
; Copyright (c) 2008-2013, The Chicken Team
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
  (uses irregex data-structures)
  (fixnum)
  (hide chop-pds absolute-pathname-root root-origin root-directory split-directory)
  (disable-interrupts) 
  (foreign-declare #<<EOF
#include <unistd.h>
#include <errno.h>

#ifndef _WIN32
# include <sys/stat.h>
# define C_mkdir(str)       C_fix(mkdir(C_c_string(str), S_IRWXU | S_IRWXG | S_IRWXO))
#else
# define C_mkdir(str)	    C_fix(mkdir(C_c_string(str)))
#endif
EOF
))

(include "common-declarations.scm")

(register-feature! 'files)


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

;;; Pathname operations:

;; Platform specific absolute pathname operations:
;; absolute-pathname-root => #f or (<match> [<origin>] <root>)
;;
;; Not for general consumption

(define absolute-pathname-root)
(define root-origin)
(define root-directory)

(if ##sys#windows-platform
    (let ((rx (irregex "([A-Za-z]:)?([\\/\\\\]).*")))
      (set! absolute-pathname-root (lambda (pn) (irregex-match rx pn)))
      (set! root-origin (lambda (rt) (and rt (irregex-match-substring rt 1))))
      (set! root-directory (lambda (rt) (and rt (irregex-match-substring rt 2)))) )
    (let ((rx (irregex "([\\/\\\\]).*")))
      (set! absolute-pathname-root (lambda (pn) (irregex-match rx pn)))
      (set! root-origin (lambda (rt) #f))
      (set! root-directory (lambda (rt) (and rt (irregex-match-substring rt 1)))) ) )

(define (absolute-pathname? pn)
  (##sys#check-string pn 'absolute-pathname?)
  (irregex-match-data? (absolute-pathname-root pn)) )

(define-inline (*char-pds? ch) (memq ch '(#\\ #\/)))

(define (chop-pds str)
  (and str
       (let ((len (##sys#size str)))
	 (if (and (fx>= len 1)
		  (*char-pds? (##core#inline "C_subchar" str (fx- len 1)) ) )
	     (##sys#substring str 0 (fx- len 1))
	     str) ) ) )

(define make-pathname)
(define make-absolute-pathname)

(let ()

  (define (conc-dirs dirs)
    (##sys#check-list dirs 'make-pathname)
    (let loop ((strs dirs))
      (if (null? strs)
	  ""
	  (let ((s1 (car strs)))
	    (if (zero? (string-length s1))
		(loop (cdr strs))
		(string-append 
		 (chop-pds (car strs))
		 "/"
		 (loop (cdr strs))) ) ) ) ) )

  (define (canonicalize-dirs dirs)
    (cond ((or (not dirs) (null? dirs)) "")
	  ((string? dirs) (conc-dirs (list dirs)))
	  (else           (conc-dirs dirs)) ) )

  (define (_make-pathname loc dir file ext)
    (let ((ext (or ext ""))
	  (file (or file "")))
      (##sys#check-string dir loc)
      (##sys#check-string file loc)
      (##sys#check-string ext loc)
      (string-append
       dir
       (if (and (fx>= (##sys#size dir) 1)
		(fx>= (##sys#size file) 1)
		(*char-pds? (##core#inline "C_subchar" file 0)))
	   (##sys#substring file 1 (##sys#size file))
	   file)
       (if (and (fx> (##sys#size ext) 0)
		(not (char=? (##core#inline "C_subchar" ext 0) #\.)) )
	   "."
	   "")
       ext) ) )

  (set! make-pathname
    (lambda (dirs file #!optional ext)
      (_make-pathname 'make-pathname (canonicalize-dirs dirs) file ext)))

  (set! make-absolute-pathname
    (lambda (dirs file #!optional ext)
      (_make-pathname
       'make-absolute-pathname
       (let ((dir (canonicalize-dirs dirs)))
	 (if (absolute-pathname? dir)
	     dir
	     (##sys#string-append "/"dir)) )
       file ext) ) ) )

(define decompose-pathname
  (let* ([patt1 "^(.*[\\/\\\\])?([^\\/\\\\]+)(\\.([^\\/\\\\.]+))$"]
	 [patt2 "^(.*[\\/\\\\])?((\\.)?[^\\/\\\\]+)$"]
	 [rx1 (irregex patt1)]
	 [rx2 (irregex patt2)]
	 [strip-pds
	  (lambda (dir)
	    (and dir
		 (if (member dir '("/" "\\"))
		     dir
		     (chop-pds dir) ) ) )] )
    (lambda (pn)
      (##sys#check-string pn 'decompose-pathname)
      (if (fx= 0 (##sys#size pn))
	  (values #f #f #f)
	  (let ([ms (irregex-search rx1 pn)])
	    (if ms
		(values 
		 (strip-pds (irregex-match-substring ms 1))
		 (irregex-match-substring ms 2)
		 (irregex-match-substring ms 4))
		(let ([ms (irregex-search rx2 pn)])
		  (if ms
		      (values
		       (strip-pds (irregex-match-substring ms 1))
		       (irregex-match-substring ms 2) 
		       #f)
		      (values (strip-pds pn) #f #f) ) ) ) ) ) ) ) )

(define pathname-directory
  (lambda (pn)
    (let-values ([(dir file ext) (decompose-pathname pn)])
      dir) ) )

(define pathname-file
  (lambda (pn)
    (let-values ([(dir file ext) (decompose-pathname pn)])
      file) ) )

(define pathname-extension
  (lambda (pn)
    (let-values ([(dir file ext) (decompose-pathname pn)])
      ext) ) )

(define pathname-strip-directory
  (lambda (pn)
    (let-values ([(dir file ext) (decompose-pathname pn)])
      (make-pathname #f file ext) ) ) )

(define pathname-strip-extension
  (lambda (pn)
    (let-values ([(dir file ext) (decompose-pathname pn)])
      (make-pathname dir file) ) ) )

(define pathname-replace-directory
  (lambda (pn dir)
    (let-values ([(_ file ext) (decompose-pathname pn)])
      (make-pathname dir file ext) ) ) )

(define pathname-replace-file
  (lambda (pn file)
    (let-values ([(dir _ ext) (decompose-pathname pn)])
      (make-pathname dir file ext) ) ) )

(define pathname-replace-extension
  (lambda (pn ext)
    (let-values ([(dir file _) (decompose-pathname pn)])
      (make-pathname dir file ext) ) ) )

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
		     pn) ))))))))


;;; normalize pathname for a particular platform

(define normalize-pathname
  (let ((bldplt (if (eq? (build-platform) 'mingw32) 'windows 'unix)) )
    (define (addpart part parts)
      (cond ((string=? "." part) parts)
            ((string=? ".." part) 
	     (if (or (null? parts)
		     (string=? ".." (car parts)))
		 (cons part parts)
		 (cdr parts)))
            (else (cons part parts) ) ) )
    (lambda (path #!optional (platform bldplt))
      (let ((sep (if (eq? platform 'windows) #\\ #\/)))
	(##sys#check-string path 'normalize-pathname)
	(let ((len (##sys#size path))
	      (abspath #f)
	      (drive #f))
	  (let loop ((i 0) (prev 0) (parts '()))
	    (cond ((fx>= i len)
		   (when (fx> i prev)
		     (set! parts (addpart (##sys#substring path prev i) parts)))
		   (if (null? parts)
		       (let ((r (if abspath
				    (##sys#string-append (string sep) ".")
				    (##sys#string-append "." (string sep)) )))
			 (if drive
			     (##sys#string-append drive r)
			     r))
		       (let ((out (open-output-string))
			     (parts (##sys#fast-reverse parts)))
			 (display (car parts) out)
			 (for-each
			  (lambda (p)
			    (##sys#write-char-0 sep out)
			    (display p out) )
			  (cdr parts))
			 (when (fx= i prev) (##sys#write-char-0 sep out))
			 (let* ((r1 (get-output-string out))
				(r (##sys#expand-home-path r1)))
			   (when (string=? r1 r)
			     (when abspath 
			       (set! r (##sys#string-append (string sep) r)))
			     (when drive
			       (set! r (##sys#string-append drive r))))
			   r))))
		  ((*char-pds? (string-ref path i))
		   (when (and (null? parts) (fx= i prev))
		     (set! abspath #t))
		   (if (fx= i prev)
		       (loop (fx+ i 1) (fx+ i 1) parts)
		       (loop (fx+ i 1)
			     (fx+ i 1)
			     (addpart (##sys#substring path prev i) parts))))
		  ((and (null? parts) 
			(char=? (string-ref path i) #\:)
			(eq? 'windows platform))
		   (set! drive (##sys#substring path 0 (fx+ i 1)))
		   (loop (fx+ i 1) (fx+ i 1) '()))
		  (else (loop (fx+ i 1) prev parts)) ) ) ) ) ) ) )


;; directory pathname => list of strings
;; does arg check

(define split-directory
  (lambda (loc dir keep?)
    (##sys#check-string dir loc)
    (string-split dir "/\\" keep?) ) )

;; Directory string or list only contains path-separators
;; and/or current-directory (".") names.

(define (directory-null? dir)
  (let loop ((ls (if (list? dir) dir (split-directory 'directory-null? dir #t))))
    (or (null? ls)
        (and (member (car ls) '("" "."))
             (loop (cdr ls)) ) ) ) )

;; Directory string => {<origin> <root> <directory-list>}
;; where any maybe #f when missing

(define (decompose-directory dir)
  (define (strip-origin-prefix org decomp)
    #;(assert (or (not org) decomp)) ;cannot have an "origin" but no "decomp"
    (if (not org)
        decomp
        (let ((1st (car decomp)))
          (let ((olen (##sys#size org)))
            (if (not (##core#inline "C_substring_compare" org 1st 0 0 olen))
                ; then origin is not a prefix (really shouldn't happen)
                decomp
                ; else is a prefix
                (let ((rst (cdr decomp))
                      (elen (##sys#size 1st)) )
                  (if (fx= olen (##sys#size elen))
                      ; then origin is a list prefix
                      rst
                      ; else origin is a string prefix
                      (cons (##sys#substring 1st olen elen) rst) ) ) ) ) ) ) )
  (let* ((ls (split-directory 'decompose-directory dir #f))
         (rt (absolute-pathname-root dir))
         (org (root-origin rt)) )
    (values org (root-directory rt) (strip-origin-prefix org (and (not (null? ls)) ls))) ) )
