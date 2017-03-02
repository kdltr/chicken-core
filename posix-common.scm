;;;; posix-common.scm - common code for UNIX and Windows versions of the posix unit
;
; Copyright (c) 2010-2017, The CHICKEN Team
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


(declare 
  (foreign-declare #<<EOF

#include <signal.h>
#include <errno.h>

#include <sys/stat.h>

static int C_not_implemented(void);
int C_not_implemented() { return -1; }

#define C_curdir(buf)       (getcwd(C_c_string(buf), 1024) ? C_fix(strlen(C_c_string(buf))) : C_SCHEME_FALSE)

static C_TLS struct stat C_statbuf;

#define C_stat_type         (C_statbuf.st_mode & S_IFMT)
#define C_stat(fn)          C_fix(stat((char *)C_data_pointer(fn), &C_statbuf))
#define C_fstat(f)          C_fix(fstat(C_unfix(f), &C_statbuf))

#ifndef S_IFSOCK
# define S_IFSOCK           0140000
#endif

#define cpy_tmvec_to_tmstc08(ptm, v) \
    ((ptm)->tm_sec = C_unfix(C_block_item((v), 0)), \
    (ptm)->tm_min = C_unfix(C_block_item((v), 1)), \
    (ptm)->tm_hour = C_unfix(C_block_item((v), 2)), \
    (ptm)->tm_mday = C_unfix(C_block_item((v), 3)), \
    (ptm)->tm_mon = C_unfix(C_block_item((v), 4)), \
    (ptm)->tm_year = C_unfix(C_block_item((v), 5)), \
    (ptm)->tm_wday = C_unfix(C_block_item((v), 6)), \
    (ptm)->tm_yday = C_unfix(C_block_item((v), 7)), \
    (ptm)->tm_isdst = (C_block_item((v), 8) != C_SCHEME_FALSE))

#define cpy_tmvec_to_tmstc9(ptm, v) \
    (((struct tm *)ptm)->tm_gmtoff = -C_unfix(C_block_item((v), 9)))

#define C_tm_set_08(v, tm)  cpy_tmvec_to_tmstc08( (tm), (v) )
#define C_tm_set_9(v, tm)   cpy_tmvec_to_tmstc9( (tm), (v) )

static struct tm *
C_tm_set( C_word v, void *tm )
{
  C_tm_set_08( v, (struct tm *)tm );
#if defined(C_GNU_ENV) && !defined(__CYGWIN__) && !defined(__uClinux__)
  C_tm_set_9( v, (struct tm *)tm );
#endif
  return tm;
}

#define TIME_STRING_MAXLENGTH 255
static char C_time_string [TIME_STRING_MAXLENGTH + 1];
#undef TIME_STRING_MAXLENGTH

#define C_strftime(v, f, tm) \
        (strftime(C_time_string, sizeof(C_time_string), C_c_string(f), C_tm_set((v), (tm))) ? C_time_string : NULL)
#define C_a_mktime(ptr, c, v, tm)  C_int64_to_num(ptr, mktime(C_tm_set((v), C_data_pointer(tm))))
#define C_asctime(v, tm)    (asctime(C_tm_set((v), (tm))))

#define C_fdopen(a, n, fd, m) C_mpointer(a, fdopen(C_unfix(fd), C_c_string(m)))
#define C_C_fileno(p)       C_fix(fileno(C_port_file(p)))
#define C_dup(x)            C_fix(dup(C_unfix(x)))
#define C_dup2(x, y)        C_fix(dup2(C_unfix(x), C_unfix(y)))

#define C_set_file_ptr(port, ptr)  (C_set_block_item(port, 0, (C_block_item(ptr, 0))), C_SCHEME_UNDEFINED)

#define C_opendir(x,h)      C_set_block_item(h, 0, (C_word) opendir(C_c_string(x)))
#define C_closedir(h)       (closedir((DIR *)C_block_item(h, 0)), C_SCHEME_UNDEFINED)
#define C_readdir(h,e)      C_set_block_item(e, 0, (C_word) readdir((DIR *)C_block_item(h, 0)))
#define C_foundfile(e,b,l)    (C_strlcpy(C_c_string(b), ((struct dirent *) C_block_item(e, 0))->d_name, l), C_fix(strlen(((struct dirent *) C_block_item(e, 0))->d_name)))

/* It is assumed that 'int' is-a 'long' */
#define C_ftell(a, n, p)    C_int64_to_num(a, ftell(C_port_file(p)))
#define C_fseek(p, n, w)    C_mk_nbool(fseek(C_port_file(p), C_num_to_int64(n), C_unfix(w)))
#define C_lseek(fd, o, w)     C_fix(lseek(C_unfix(fd), C_num_to_int64(o), C_unfix(w)))

#ifdef HAVE_SETENV
# define C_unsetenv(s)      (unsetenv((char *)C_data_pointer(s)), C_SCHEME_TRUE)
# define C_setenv(x, y)     C_fix(setenv((char *)C_data_pointer(x), (char *)C_data_pointer(y), 1))
#else
# if defined(_WIN32) && !defined(__CYGWIN__)
#  define C_unsetenv(s)      C_setenv(s, C_SCHEME_FALSE)
# else
#  define C_unsetenv(s)      C_fix(putenv((char *)C_data_pointer(s)))
# endif
static C_word C_fcall C_setenv(C_word x, C_word y) {
  char *sx = C_c_string(x),
       *sy = (y == C_SCHEME_FALSE ? "" : C_c_string(y));
  int n1 = C_strlen(sx), n2 = C_strlen(sy);
  int buf_len = n1 + n2 + 2;
  char *buf = (char *)C_malloc(buf_len);
  if(buf == NULL) return(C_fix(0));
  else {
    C_strlcpy(buf, sx, buf_len);
    C_strlcat(buf, "=", buf_len);
    C_strlcat(buf, sy, buf_len);
    return(C_fix(putenv(buf)));
  }
}
#endif

EOF
))

(include "common-declarations.scm")


;;; Error codes:

(define-foreign-variable _errno int "errno")

(define-foreign-variable _eperm int "EPERM")
(define-foreign-variable _enoent int "ENOENT")
(define-foreign-variable _esrch int "ESRCH")
(define-foreign-variable _eintr int "EINTR")
(define-foreign-variable _eio int "EIO")
(define-foreign-variable _enoexec int "ENOEXEC")
(define-foreign-variable _ebadf int "EBADF")
(define-foreign-variable _echild int "ECHILD")
(define-foreign-variable _enomem int "ENOMEM")
(define-foreign-variable _eacces int "EACCES")
(define-foreign-variable _efault int "EFAULT")
(define-foreign-variable _ebusy int "EBUSY")
(define-foreign-variable _eexist int "EEXIST")
(define-foreign-variable _enotdir int "ENOTDIR")
(define-foreign-variable _eisdir int "EISDIR")
(define-foreign-variable _einval int "EINVAL")
(define-foreign-variable _emfile int "EMFILE")
(define-foreign-variable _enospc int "ENOSPC")
(define-foreign-variable _espipe int "ESPIPE")
(define-foreign-variable _epipe int "EPIPE")
(define-foreign-variable _eagain int "EAGAIN")
(define-foreign-variable _erofs int "EROFS")
(define-foreign-variable _enxio int "ENXIO")
(define-foreign-variable _e2big int "E2BIG")
(define-foreign-variable _exdev int "EXDEV")
(define-foreign-variable _enodev int "ENODEV")
(define-foreign-variable _enfile int "ENFILE")
(define-foreign-variable _enotty int "ENOTTY")
(define-foreign-variable _efbig int "EFBIG")
(define-foreign-variable _emlink int "EMLINK")
(define-foreign-variable _edom int "EDOM")
(define-foreign-variable _erange int "ERANGE")
(define-foreign-variable _edeadlk int "EDEADLK")
(define-foreign-variable _enametoolong int "ENAMETOOLONG")
(define-foreign-variable _enolck int "ENOLCK")
(define-foreign-variable _enosys int "ENOSYS")
(define-foreign-variable _enotempty int "ENOTEMPTY")
(define-foreign-variable _eilseq int "EILSEQ")
(define-foreign-variable _ewouldblock int "EWOULDBLOCK")

(define posix-error
  (let ([strerror (foreign-lambda c-string "strerror" int)]
	[string-append string-append] )
    (lambda (type loc msg . args)
      (let ([rn (##sys#update-errno)])
	(apply ##sys#signal-hook type loc (string-append msg " - " (strerror rn)) args) ) ) ) )

(define ##sys#posix-error posix-error)


;;; Exit

(define emergency-exit
  (let ((_exit (foreign-lambda void "_exit" int)))
    (lambda (#!optional (code 0))
      (_exit code))))


;;; File properties

(define-foreign-variable _stat_st_ino unsigned-int "C_statbuf.st_ino")
(define-foreign-variable _stat_st_nlink unsigned-int "C_statbuf.st_nlink")
(define-foreign-variable _stat_st_gid unsigned-int "C_statbuf.st_gid")
(define-foreign-variable _stat_st_size integer64 "C_statbuf.st_size")
(define-foreign-variable _stat_st_mtime integer64 "C_statbuf.st_mtime")
(define-foreign-variable _stat_st_atime integer64 "C_statbuf.st_atime")
(define-foreign-variable _stat_st_ctime integer64 "C_statbuf.st_ctime")
(define-foreign-variable _stat_st_uid unsigned-int "C_statbuf.st_uid")
(define-foreign-variable _stat_st_mode unsigned-int "C_statbuf.st_mode")
(define-foreign-variable _stat_st_dev unsigned-int "C_statbuf.st_dev")
(define-foreign-variable _stat_st_rdev unsigned-int "C_statbuf.st_rdev")

(define-syntax stat-mode
  (er-macro-transformer
   (lambda (x r c)
     ;; no need to rename here
     (let* ((mode (cadr x))
	    (name (symbol->string mode)))
       `(##core#begin
	 (declare
	   (foreign-declare
	     ,(string-append "#ifndef " name "\n"
			     "#define " name " S_IFREG\n"
			     "#endif\n")))
	 (define-foreign-variable ,mode unsigned-int))))))

(stat-mode S_IFLNK)
(stat-mode S_IFREG)
(stat-mode S_IFDIR)
(stat-mode S_IFCHR)
(stat-mode S_IFBLK)
(stat-mode S_IFSOCK)
(stat-mode S_IFIFO)

(define (##sys#stat file link err loc)
  (let ((r (cond ((fixnum? file) (##core#inline "C_fstat" file))
                 ((port? file) (##core#inline "C_fstat" (port->fileno file)))
                 ((string? file)
                  (let ((path (##sys#make-c-string
			       (##sys#platform-fixup-pathname
                                file)
			       loc)))
		    (if link
			(##core#inline "C_lstat" path)
			(##core#inline "C_stat" path) ) ) )
                 (else
		  (##sys#signal-hook
		   #:type-error loc "bad argument type - not a fixnum or string" file)) ) ) )
    (if (fx< r 0)
	(if err
	    (posix-error #:file-error loc "cannot access file" file) 
	    #f)
	#t)))

(define (file-stat f #!optional link)
  (##sys#stat f link #t 'file-stat)
  (vector _stat_st_ino _stat_st_mode _stat_st_nlink
          _stat_st_uid _stat_st_gid _stat_st_size
          _stat_st_atime _stat_st_ctime _stat_st_mtime
          _stat_st_dev _stat_st_rdev
          _stat_st_blksize _stat_st_blocks) )

(define file-modification-time
  (getter-with-setter 
   (lambda (f)
     (##sys#stat f #f #t 'file-modification-time) _stat_st_mtime)
   (lambda (f t)
     (##sys#check-exact-integer t 'set-file-modification-time)
     (let ((r ((foreign-lambda int "set_file_mtime" c-string scheme-object)
	       f t)))
       (when (fx< r 0)
	 (posix-error 
	  #:file-error 'set-file-modification-time
	  "cannot set file modification-time" f t))))
   "(file-modification-time f)"))

(define (file-access-time f) (##sys#stat f #f #t 'file-access-time) _stat_st_atime)
(define (file-change-time f) (##sys#stat f #f #t 'file-change-time) _stat_st_ctime)
(define (file-owner f) (##sys#stat f #f #t 'file-owner) _stat_st_uid)
(define (file-permissions f) (##sys#stat f #f #t 'file-permissions) _stat_st_mode)
(define (file-size f) (##sys#stat f #f #t 'file-size) _stat_st_size)

(define (file-type file #!optional link (err #t))
  (and (##sys#stat file link err 'file-type)
       (select (foreign-value "C_stat_type" unsigned-int)
	 ((S_IFREG) 'regular-file)
	 ((S_IFLNK) 'symbolic-link)
	 ((S_IFDIR) 'directory)
	 ((S_IFCHR) 'character-device)
	 ((S_IFBLK) 'block-device)
	 ((S_IFIFO) 'fifo)
	 ((S_IFSOCK) 'socket)
	 (else 'regular-file))))

(define (regular-file? file)
  (eq? 'regular-file (file-type file #f #f)))

(define (symbolic-link? file)
  (eq? 'symbolic-link (file-type file #t #f)))

(define (block-device? file)
  (eq? 'block-device (file-type file #f #f)))

(define (character-device? file)
  (eq? 'character-device (file-type file #f #f)))

(define (fifo? file)
  (eq? 'fifo (file-type file #f #f)))

(define (socket? file)
  (eq? 'socket (file-type file #f #f)))

(define (directory? file)
  (eq? 'directory (file-type file #f #f)))


;;; File position access:

(define-foreign-variable _seek_set int "SEEK_SET")
(define-foreign-variable _seek_cur int "SEEK_CUR")
(define-foreign-variable _seek_end int "SEEK_END")

(define seek/set _seek_set)
(define seek/end _seek_end)
(define seek/cur _seek_cur)

(define set-file-position!
  (lambda (port pos . whence)
    (let ((whence (if (pair? whence) (car whence) _seek_set)))
      (##sys#check-fixnum pos 'set-file-position!)
      (##sys#check-fixnum whence 'set-file-position!)
      (unless (cond ((port? port)
		     (and (eq? (##sys#slot port 7) 'stream)
			  (##core#inline "C_fseek" port pos whence) ) )
		    ((fixnum? port)
		     (##core#inline "C_lseek" port pos whence))
		    (else
		     (##sys#signal-hook #:type-error 'set-file-position! "invalid file" port)) )
	(posix-error #:file-error 'set-file-position! "cannot set file position" port pos) ) ) ) )

(define file-position
  (getter-with-setter
   (lambda (port)
     (let ((pos (cond ((port? port)
		       (if (eq? (##sys#slot port 7) 'stream)
			   (##core#inline_allocate ("C_ftell" 7) port)
			   -1) )
		      ((fixnum? port)
		       (##core#inline "C_lseek" port 0 _seek_cur) )
		      (else
		       (##sys#signal-hook #:type-error 'file-position "invalid file" port)) ) ) )
       (when (< pos 0)
	 (posix-error #:file-error 'file-position "cannot retrieve file position of port" port) )
       pos) )
   set-file-position!		; doesn't accept WHENCE
   "(file-position port)"))


;;; Using file-descriptors:

(define-foreign-variable _stdin_fileno int "STDIN_FILENO")
(define-foreign-variable _stdout_fileno int "STDOUT_FILENO")
(define-foreign-variable _stderr_fileno int "STDERR_FILENO")

(define fileno/stdin _stdin_fileno)
(define fileno/stdout _stdout_fileno)
(define fileno/stderr _stderr_fileno)

(define open-input-file*)
(define open-output-file*)

(let ()
  (define (mode inp m loc)
    (##sys#make-c-string
     (cond [(pair? m)
            (let ([m (car m)])
              (case m
                [(###append) (if (not inp) "a" (##sys#error "invalid mode for input file" m))]
                [else (##sys#error "invalid mode argument" m)] ) ) ]
           [inp "r"]
           [else "w"] )
     loc) )
  (define (check loc fd inp r)
    (if (##sys#null-pointer? r)
        (posix-error #:file-error loc "cannot open file" fd)
        (let ((port (##sys#make-port (if inp 1 2) ##sys#stream-port-class "(fdport)" 'stream)))
          (##core#inline "C_set_file_ptr" port r)
          port) ) )
  (set! open-input-file*
    (lambda (fd . m)
      (##sys#check-fixnum fd 'open-input-file*)
      (check 'open-input-file* fd #t (##core#inline_allocate ("C_fdopen" 2) fd (mode #t m 'open-input-file*))) ) )
  (set! open-output-file*
    (lambda (fd . m)
      (##sys#check-fixnum fd 'open-output-file*)
      (check 'open-output-file* fd #f (##core#inline_allocate ("C_fdopen" 2) fd (mode #f m 'open-output-file*)) ) ) ) )

(define port->fileno
  (lambda (port)
    (##sys#check-open-port port 'port->fileno)
    (cond [(eq? 'socket (##sys#slot port 7))
	   ;; Extract socket-FD from the port's "data" object - this is identical
	   ;; to "##sys#tcp-port->fileno" in the tcp unit (tcp.scm). We code it in
	   ;; this low-level manner to avoid depend on code defined there.
	   ;; Peter agrees with that. I think. Have a nice day.
	   (##sys#slot (##sys#port-data port) 0) ]
          [(not (zero? (##sys#peek-unsigned-integer port 0)))
           (let ([fd (##core#inline "C_C_fileno" port)])
             (when (fx< fd 0)
               (posix-error #:file-error 'port->fileno "cannot access file-descriptor of port" port) )
             fd) ]
          [else (posix-error #:type-error 'port->fileno "port has no attached file" port)] ) ) )

(define duplicate-fileno
  (lambda (old . new)
    (##sys#check-fixnum old duplicate-fileno)
    (let ([fd (if (null? new)
                  (##core#inline "C_dup" old)
                  (let ([n (car new)])
                    (##sys#check-fixnum n 'duplicate-fileno)
                    (##core#inline "C_dup2" old n) ) ) ] )
      (when (fx< fd 0)
        (posix-error #:file-error 'duplicate-fileno "cannot duplicate file-descriptor" old) )
      fd) ) )


;;; Set or get current directory:

(define (current-directory #!optional dir)
  (if dir
      (change-directory dir)
      (let* ((buffer (make-string 1024))
	     (len (##core#inline "C_curdir" buffer)) )
	#+(or unix cygwin)
	(##sys#update-errno)
	(if len
	    (##sys#substring buffer 0 len)
	    (##sys#signal-hook
	     #:file-error
	     'current-directory "cannot retrieve current directory") ) ) ) )

(define delete-directory
  (lambda (name #!optional recursive)
    (define (rmdir dir)
      (let ((sname (##sys#make-c-string dir)))
	(unless (fx= 0 (##core#inline "C_rmdir" sname))
	  (posix-error #:file-error 'delete-directory "cannot delete directory" dir) )))
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

(define-inline (*create-directory loc name)
  (unless (fx= 0 (##core#inline "C_mkdir" (##sys#make-c-string name loc)))
    (posix-error #:file-error loc "cannot create directory" name)) )

(define create-directory
  (lambda (name #!optional parents?)
    (##sys#check-string name 'create-directory)
    (unless (or (fx= 0 (##sys#size name))
                (file-exists? name))
      (if parents?
        (let loop ((dir (let-values (((dir file ext) (decompose-pathname name)))
                          (if file (make-pathname dir file ext) dir))))
          (when (and dir (not (directory? dir)))
            (loop (pathname-directory dir))
            (*create-directory 'create-directory dir)) )
        (*create-directory 'create-directory name) ) )
    name))

(define directory
  (lambda (#!optional (spec (current-directory)) show-dotfiles?)
    (##sys#check-string spec 'directory)
    (let ([buffer (make-string 256)]
	  [handle (##sys#make-pointer)]
	  [entry (##sys#make-pointer)] )
      (##core#inline 
       "C_opendir"
       (##sys#make-c-string spec 'directory) handle)
      (if (##sys#null-pointer? handle)
	  (posix-error #:file-error 'directory "cannot open directory" spec)
	  (let loop ()
	    (##core#inline "C_readdir" handle entry)
	    (if (##sys#null-pointer? entry)
		(begin
		  (##core#inline "C_closedir" handle)
		  '() )
		(let* ([flen (##core#inline "C_foundfile" entry buffer (string-length buffer))]
		       [file (##sys#substring buffer 0 flen)]
		       [char1 (string-ref file 0)]
		       [char2 (and (fx> flen 1) (string-ref file 1))] )
		  (if (and (eq? #\. char1)
			   (or (not char2)
			       (and (eq? #\. char2) (eq? 2 flen))
			       (not show-dotfiles?) ) )
		      (loop)
		      (cons file (loop)) ) ) ) ) ) ) ) )

;;; Filename globbing:

(define glob
  (lambda paths
    (let conc-loop ((paths paths))
      (if (null? paths)
	  '()
	  (let ((path (car paths)))
	    (let-values (((dir fil ext) (decompose-pathname path)))
	      (let ((rx (##sys#glob->regexp (make-pathname #f (or fil "*") ext))))
		(let loop ((fns (directory (or dir ".") #t)))
		  (cond ((null? fns) (conc-loop (cdr paths)))
			((irregex-match rx (car fns))
			 => (lambda (m)
			      (cons 
			       (make-pathname dir (irregex-match-substring m))
			       (loop (cdr fns)))) )
			(else (loop (cdr fns))) ) ) ) ) ) ) ) ) )


;;; Find matching files:

(define (##sys#find-files dir pred action id limit follow dot loc)
  (##sys#check-string dir loc)
  (let* ((depth 0)
         (lproc
          (cond ((not limit) (lambda _ #t))
                ((fixnum? limit) (lambda _ (fx< depth limit)))
                (else limit) ) )
         (pproc
          (if (procedure? pred)
              pred
              (let ((pred (irregex pred))) ; force compilation
                (lambda (x) (irregex-match pred x))))))
    (let loop ((dir dir)
               (fs (directory dir dot))
               (r id))
      (if (null? fs)
          r
          (let* ((filename (##sys#slot fs 0))
                 (f (make-pathname dir filename))
                 (rest (##sys#slot fs 1)))
            (cond ((directory? f)
                   (cond ((member filename '("." "..")) (loop dir rest r))
                         ((and (symbolic-link? f) (not follow))
                          (loop dir rest (if (pproc f) (action f r) r)))
                         ((lproc f)
                          (loop dir
                                rest
                                (fluid-let ((depth (fx+ depth 1)))
                                  (loop f
                                        (directory f dot)
                                        (if (pproc f) (action f r) r)))))
                         (else (loop dir rest (if (pproc f) (action f r) r)))))
                  ((pproc f) (loop dir rest (action f r)))
                  (else (loop dir rest r))))))))

(define (find-files dir #!key (test (lambda _ #t))
			      (action (lambda (x y) (cons x y)))
                              (seed '())
                              (limit #f)
                              (dotfiles #f)
                              (follow-symlinks #f))
  (##sys#find-files dir test action seed limit follow-symlinks dotfiles 'find-files))


;;; umask

(define file-creation-mode
  (getter-with-setter
   (lambda (#!optional um)
     (when um (##sys#check-fixnum um 'file-creation-mode))
     (let ((um2 (##core#inline "C_umask" (or um 0))))
       (unless um (##core#inline "C_umask" um2)) ; restore
       um2))
   (lambda (um)
     (##sys#check-fixnum um 'file-creation-mode)
     (##core#inline "C_umask" um))
   "(file-creation-mode mode)"))


;;; Time related things:

(define (check-time-vector loc tm)
  (##sys#check-vector tm loc)
  (when (fx< (##sys#size tm) 10)
    (##sys#error loc "time vector too short" tm) ) )

(define (seconds->local-time #!optional (secs (current-seconds)))
  (##sys#check-exact-integer secs 'seconds->local-time)
  (##sys#decode-seconds secs #f) )

(define (seconds->utc-time #!optional (secs (current-seconds)))
  (##sys#check-exact-integer secs 'seconds->utc-time)
  (##sys#decode-seconds secs #t) )

(define seconds->string
  (let ([ctime (foreign-lambda c-string "C_ctime" integer)])
    (lambda (#!optional (secs (current-seconds)))
      (##sys#check-exact-integer secs 'seconds->string)
      (let ([str (ctime secs)])
        (if str
            (##sys#substring str 0 (fx- (##sys#size str) 1))
            (##sys#error 'seconds->string "cannot convert seconds to string" secs) ) ) ) ) )

(define local-time->seconds
  (let ((tm-size (foreign-value "sizeof(struct tm)" int)))
    (lambda (tm)
      (check-time-vector 'local-time->seconds tm)
      (let ((t (##core#inline_allocate ("C_a_mktime" 7) tm (##sys#make-string tm-size #\nul))))
        (if (= -1 t)
            (##sys#error 'local-time->seconds "cannot convert time vector to seconds" tm)
            t)))))

(define time->string
  (let ((asctime (foreign-lambda c-string "C_asctime" scheme-object scheme-pointer))
        (strftime (foreign-lambda c-string "C_strftime" scheme-object scheme-object scheme-pointer))
        (tm-size (foreign-value "sizeof(struct tm)" int)))
    (lambda (tm #!optional fmt)
      (check-time-vector 'time->string tm)
      (if fmt
          (begin
            (##sys#check-string fmt 'time->string)
            (or (strftime tm (##sys#make-c-string fmt 'time->string) (##sys#make-string tm-size #\nul))
                (##sys#error 'time->string "time formatting overflows buffer" tm)) )
          (let ([str (asctime tm (##sys#make-string tm-size #\nul))])
            (if str
                (##sys#substring str 0 (fx- (##sys#size str) 1))
                (##sys#error 'time->string "cannot convert time vector to string" tm) ) ) ) ) ) )


;;; Environment access:

(define set-environment-variable!
  (lambda (var val)
    (##sys#check-string var 'set-environment-variable!)
    (##sys#check-string val 'set-environment-variable!)
    (##core#inline "C_setenv"
     (##sys#make-c-string var 'set-environment-variable!)
     (##sys#make-c-string val 'set-environment-variable!))
    (##core#undefined) ) )

(define (unset-environment-variable! var)
  (##sys#check-string var 'unset-environment-variable!)
  (##core#inline "C_unsetenv"
   (##sys#make-c-string var 'unset-environment-variable!))
  (##core#undefined) )

(define get-environment-variables
  (let ([get (foreign-lambda c-string "C_getenventry" int)])
    (lambda ()
      (let loop ([i 0])
        (let ([entry (get i)])
          (if entry
              (let scan ([j 0])
                (if (char=? #\= (##core#inline "C_subchar" entry j))
                    (cons (cons (##sys#substring entry 0 j)
                                (##sys#substring entry (fx+ j 1) (##sys#size entry)))
                          (loop (fx+ i 1)))
                    (scan (fx+ j 1)) ) )
              '() ) ) ) ) ) )


;;; Signals

(define (set-signal-handler! sig proc)
  (##sys#check-fixnum sig 'set-signal-handler!)
  (##core#inline "C_establish_signal_handler" sig (and proc sig))
  (vector-set! ##sys#signal-vector sig proc) )

(define signal-handler
  (getter-with-setter
   (lambda (sig)
     (##sys#check-fixnum sig 'signal-handler)
     (##sys#slot ##sys#signal-vector sig) )
   set-signal-handler!))


;;; Processes

(define current-process-id (foreign-lambda int "C_getpid"))

(define (process-sleep n)
  (##sys#check-fixnum n 'process-sleep)
  (##core#inline "C_process_sleep" n))

(define process-wait
  (lambda args
    (let-optionals* args ([pid #f] [nohang #f])
      (let ([pid (or pid -1)])
        (##sys#check-fixnum pid 'process-wait)
        (receive [epid enorm ecode] (##sys#process-wait pid nohang)
          (if (fx= epid -1)
              (posix-error #:process-error 'process-wait "waiting for child process failed" pid)
              (values epid enorm ecode) ) ) ) ) ) )

;; This can construct argv or envp for process-execute or process-run
(define list->c-string-buffer
  (let ((c-string->allocated-pointer
	 (foreign-lambda* c-pointer ((scheme-object o))
	   "char *ptr = C_malloc(C_header_size(o)); \n"
	   "if (ptr != NULL) {\n"
	   "  C_memcpy(ptr, C_data_pointer(o), C_header_size(o)); \n"
	   "}\n"
	   "C_return(ptr);")))
    (lambda (string-list convert loc)
      (##sys#check-list string-list loc)

      (let* ((string-count (##sys#length string-list))
	     ;; NUL-terminated, so we must add one
	     (buffer (make-pointer-vector (add1 string-count) #f)))

	(handle-exceptions exn
	    ;; Free to avoid memory leak, then reraise
	    (begin (free-c-string-buffer buffer) (signal exn))

	  (do ((sl string-list (cdr sl))
	       (i 0 (fx+ i 1)))
	      ((or (null? sl) (fx= i string-count))) ; Should coincide

	    (##sys#check-string (car sl) loc)
	    ;; This avoids embedded NULs and appends a NUL, so "cs" is
	    ;; safe to copy and use as-is in the pointer-vector.
	    (let* ((cs (##sys#make-c-string (convert (car sl)) loc))
		   (csp (c-string->allocated-pointer cs)))
	      (unless csp (error loc "Out of memory"))
	      (pointer-vector-set! buffer i csp)))

	  buffer)))))

(define (free-c-string-buffer buffer-array)
  (let ((size (pointer-vector-length buffer-array)))
    (do ((i 0 (fx+ i 1)))
	((fx= i size))
      (and-let* ((s (pointer-vector-ref buffer-array i)))
	(free s)))))

;; Environments are represented as string->string association lists
(define (check-environment-list lst loc)
  (##sys#check-list lst loc)
  (for-each
    (lambda (p)
      (##sys#check-pair p loc)
      (##sys#check-string (car p) loc)
      (##sys#check-string (cdr p) loc))
    lst))

(define call-with-exec-args
  (let ((pathname-strip-directory pathname-strip-directory)
	(nop (lambda (x) x)))
    (lambda (loc filename argconv arglist envlist proc)
      (let* ((stripped-filename (pathname-strip-directory filename))
	     (args (cons stripped-filename arglist)) ; Add argv[0]
	     (argbuf (list->c-string-buffer args argconv loc))
	     (envbuf #f))

	(handle-exceptions exn
	    ;; Free to avoid memory leak, then reraise
	    (begin (free-c-string-buffer argbuf)
		   (when envbuf (free-c-string-buffer envbuf))
		   (signal exn))

	  ;; Envlist is never converted, so we always use nop here
	  (when envlist
	    (set! envbuf
	      (list->c-string-buffer
		(map (lambda (p) (string-append (car p) "=" (cdr p))) envlist)
		nop loc)))

	  (proc (##sys#make-c-string filename loc) argbuf envbuf))))))
