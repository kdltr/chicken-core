;;;; file.scm - File operations
;
; Copyright (c) 2008-2018, The CHICKEN Team
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

#define C_test_access(fn, m) C_fix(access((char *)C_data_pointer(fn), C_unfix(m)))

/* For Windows */
#ifndef R_OK
# define R_OK 2
#endif
#ifndef W_OK
# define W_OK 4
#endif
#ifndef X_OK
# define X_OK 2
#endif

#define C_rmdir(str)        C_fix(rmdir(C_c_string(str)))

#ifndef _WIN32
# include <sys/stat.h>
# define C_mkdir(str)       C_fix(mkdir(C_c_string(str), S_IRWXU | S_IRWXG | S_IRWXO))
#else
# define C_mkdir(str)       C_fix(mkdir(C_c_string(str)))
#endif

#if !defined(_WIN32) || defined(__CYGWIN__)
# include <sys/types.h>
# include <dirent.h>
#else
struct dirent
{
    char *              d_name;
};

typedef struct
{
    struct _finddata_t  fdata;
    int                 handle;
    struct dirent       current;
} DIR;

static DIR * C_fcall
opendir(const char *name)
{
    int name_len = strlen(name);
    int what_len = name_len + 3;
    DIR *dir = (DIR *)malloc(sizeof(DIR));
    char *what;
    if (!dir)
    {
	errno = ENOMEM;
	return NULL;
    }
    what = (char *)malloc(what_len);
    if (!what)
    {
	free(dir);
	errno = ENOMEM;
	return NULL;
    }
    C_strlcpy(what, name, what_len);
    if (strchr("\\/", name[name_len - 1]))
	C_strlcat(what, "*", what_len);
    else
	C_strlcat(what, "\\*", what_len);

    dir->handle = _findfirst(what, &dir->fdata);
    if (dir->handle == -1)
    {
	free(what);
	free(dir);
	return NULL;
    }
    dir->current.d_name = NULL; /* as the first-time indicator */
    free(what);
    return dir;
}

static int C_fcall
closedir(DIR * dir)
{
    if (dir)
    {
	int res = _findclose(dir->handle);
	free(dir);
	return res;
    }
    return -1;
}

static struct dirent * C_fcall
readdir(DIR * dir)
{
    if (dir)
    {
	if (!dir->current.d_name /* first time after opendir */
	     || _findnext(dir->handle, &dir->fdata) != -1)
	{
	    dir->current.d_name = dir->fdata.name;
	    return &dir->current;
	}
    }
    return NULL;
}
#endif

#define C_opendir(s,h)      C_set_block_item(h, 0, (C_word) opendir(C_c_string(s)))
#define C_readdir(h,e)      C_set_block_item(e, 0, (C_word) readdir((DIR *)C_block_item(h, 0)))
#define C_closedir(h)       (closedir((DIR *)C_block_item(h, 0)), C_SCHEME_UNDEFINED)
#define C_foundfile(e,b,l)  (C_strlcpy(C_c_string(b), ((struct dirent *) C_block_item(e, 0))->d_name, l), C_fix(strlen(((struct dirent *) C_block_item(e, 0))->d_name)))

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

(import scheme
	chicken.base
	chicken.condition
	chicken.fixnum
	chicken.foreign
	chicken.io
	chicken.irregex
	chicken.pathname
	chicken.process-context
	chicken.posix) ; FIXME file should not depend on posix

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

(define (file-exists? name)
  (##sys#check-string name 'file-exists?)
  (and (##sys#file-exists? name #f #f 'file-exists?) name))

(define (directory-exists? name)
  (##sys#check-string name 'directory-exists?)
  (and (##sys#file-exists? name #f #t 'directory-exists?) name))

(define (delete-file filename)
  (##sys#check-string filename 'delete-file)
  (unless (eq? 0 (##core#inline "C_delete_file" (##sys#make-c-string filename 'delete-file)))
    (##sys#update-errno)
    (##sys#signal-hook
     #:file-error 'delete-file
     (##sys#string-append "cannot delete file - " strerror) filename))
  filename)

;;; Like `delete-file', but does nothing if the file doesn't exist:

(define delete-file*
  (lambda (file)
    (and (file-exists? file) (delete-file file))))

(define (rename-file old new)
  (##sys#check-string old 'rename-file)
  (##sys#check-string new 'rename-file)
  (unless (eq? 0 (##core#inline "C_rename_file" (##sys#make-c-string old 'rename-file) (##sys#make-c-string new)))
    (##sys#update-errno)
    (##sys#signal-hook
     #:file-error 'rename-file
     (##sys#string-append "cannot rename file - " strerror) old new))
  new)


;;; Permissions:

(define-foreign-variable _r_ok int "R_OK")
(define-foreign-variable _w_ok int "W_OK")
(define-foreign-variable _x_ok int "X_OK")

(define (test-access filename acc loc)
  (##sys#check-string filename loc)
  (let ((r (##core#inline "C_test_access" (##sys#make-c-string filename loc) acc)))
    (or (fx= r 0)
	(if (fx= (##sys#update-errno) (foreign-value "EACCES" int))
	    #f
	    (posix-error #:file-error loc "cannot access file" filename)))))

(define (file-read-access? filename) (test-access filename _r_ok 'file-read-access?))
(define (file-write-access? filename) (test-access filename _w_ok 'file-write-access?))
(define (file-execute-access? filename) (test-access filename _x_ok 'file-execute-access?))


;;; Directories:

(define (directory #!optional (spec (current-directory)) show-dotfiles?)
  (##sys#check-string spec 'directory)
  (let ((buffer (make-string 256))
	(handle (##sys#make-pointer))
	(entry (##sys#make-pointer)))
    (##core#inline
     "C_opendir"
     (##sys#make-c-string spec 'directory) handle)
    (if (##sys#null-pointer? handle)
	(posix-error #:file-error 'directory "cannot open directory" spec)
	(let loop ()
	  (##core#inline "C_readdir" handle entry)
	  (if (##sys#null-pointer? entry)
	      (begin (##core#inline "C_closedir" handle) '())
	      (let* ((flen (##core#inline "C_foundfile" entry buffer (string-length buffer)))
		     (file (##sys#substring buffer 0 flen))
		     (char1 (string-ref file 0))
		     (char2 (and (fx> flen 1) (string-ref file 1))))
		(if (and (eq? #\. char1)
			 (or (not char2)
			     (and (eq? #\. char2) (eq? 2 flen))
			     (not show-dotfiles?)))
		    (loop)
		    (cons file (loop)))))))))

(define-inline (*create-directory loc name)
  (unless (fx= 0 (##core#inline "C_mkdir" (##sys#make-c-string name loc)))
    (posix-error #:file-error loc "cannot create directory" name)))

(define create-directory
  (lambda (name #!optional recursive)
    (##sys#check-string name 'create-directory)
    (unless (or (fx= 0 (##sys#size name))
                (file-exists? name))
      (if recursive
	  (let loop ((dir (let-values (((dir file ext) (decompose-pathname name)))
			    (if file (make-pathname dir file ext) dir))))
	    (when (and dir (not (directory? dir)))
	      (loop (pathname-directory dir))
	      (*create-directory 'create-directory dir)))
	  (*create-directory 'create-directory name)))
    name))

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

(define (glob . paths)
  (let conc-loop ((paths paths))
    (if (null? paths)
	'()
	(let ((path (car paths)))
	  (let-values (((dir fil ext) (decompose-pathname path)))
	    (let ((dir* (or dir "."))
		  (rx   (irregex (glob->sre (make-pathname #f (or fil "*") ext)))))
	      (let loop ((fns (condition-case (directory dir* #t)
				((exn i/o file) #f))))
		(cond ((not (pair? fns)) (conc-loop (cdr paths)))
		      ((irregex-match rx (car fns)) =>
		       (lambda (m)
			 (cons (make-pathname dir (irregex-match-substring m))
			       (loop (cdr fns)))))
		      (else (loop (cdr fns)))))))))))


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
