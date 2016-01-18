;;;; setup-api.scm - build + installation API for eggs
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


(require-library irregex utils posix extras ports data-structures files)

; This code is partially quite messy and the API is not overly consistent,
; mainly because it has grown "organically" while the old chicken-setup program
; evolved. The code was extracted and put into this module, without much
; cleaning up.
;
; *windows-shell* and, to a lesser extent, 'sudo' processing knowledge is
; scattered in the code.

(module setup-api

    ((run execute)
     compile
     standard-extension
     host-extension
     install-extension install-program install-script
     setup-verbose-mode setup-install-mode deployment-mode
     installation-prefix
     destination-prefix
     runtime-prefix
     chicken-prefix
     find-library find-header 
     program-path remove-file* 
     patch abort-setup
     setup-root-directory create-directory/parents
     test-compile try-compile run-verbose
     extra-features extra-nonfeatures
     copy-file move-file
     sudo-install keep-intermediates
     version>=?
     extension-name-and-version
     extension-name
     extension-version
     remove-directory
     remove-extension
     read-info
     register-program find-program
     shellpath
     setup-error-handling)
  
  (import scheme chicken
	  chicken.data-structures
	  chicken.extras
	  chicken.files
	  chicken.foreign
	  chicken.format
	  chicken.ports
	  chicken.posix
	  chicken.irregex
	  chicken.utils)

(include "mini-srfi-1.scm")

;;; Constants, variables and parameters

(define-constant setup-file-extension "setup-info")

(define *cc* (foreign-value "C_TARGET_CC" c-string))
(define *cxx* (foreign-value "C_TARGET_CXX" c-string))
(define *target-cflags* (foreign-value "C_TARGET_CFLAGS" c-string))
(define *target-libs* (foreign-value "C_TARGET_MORE_LIBS" c-string))
(define *target-lib-home* (foreign-value "C_TARGET_LIB_HOME" c-string))
(define *sudo* #f)
(define *windows-shell* (foreign-value "C_WINDOWS_SHELL" bool))
(define *registered-programs* '())

(define *windows*
  (and (eq? (software-type) 'windows) 
       (build-platform) ) )

(register-feature! 'chicken-setup)

(define host-extension (make-parameter #f))

(define *chicken-bin-path*
  (or (and-let* ((p (get-environment-variable "CHICKEN_PREFIX")))
	(make-pathname p "bin") )
      (foreign-value "C_INSTALL_BIN_HOME" c-string) ) )

(define chicken-prefix			
  (or (get-environment-variable "CHICKEN_PREFIX")
      (foreign-value "C_INSTALL_PREFIX" c-string)))

(define (shellpath str)
  (qs (normalize-pathname str)))

(define *csc-options* '())
(define *base-directory* (current-directory))

(define setup-root-directory (make-parameter *base-directory*))
(define setup-verbose-mode   (make-parameter #f))
(define setup-install-mode   (make-parameter #t))
(define deployment-mode      (make-parameter #f))
(define program-path         (make-parameter *chicken-bin-path*))
(define keep-intermediates   (make-parameter #f))

(define extra-features
  (let ((xfs '()))
    (lambda (#!optional fs)
      (cond (fs (apply register-feature! fs)
		(set! xfs fs))
	    (else xfs)))))

(define extra-nonfeatures
  (let ((xfs '()))
    (lambda (#!optional fs)
      (cond (fs (apply unregister-feature! fs)
		(set! xfs fs))
	    (else xfs)))))

; Setup shell commands

(define *copy-command*)
(define *remove-command*)
(define *move-command*)
(define *chmod-command*)
(define *ranlib-command*)
(define *mkdir-command*)

(define (windows-user-install-setup)
  (set! *copy-command*        "copy")
  (set! *remove-command*      "del /Q /S")
  (set! *move-command*        "move")
  (set! *chmod-command*       "chmod")
  (set! *ranlib-command*      "ranlib") )

(define (unix-user-install-setup)
  (set! *copy-command*        "cp -r")
  (set! *remove-command*      "rm -fr")
  (set! *move-command*        "mv")
  (set! *chmod-command*       "chmod")
  (set! *ranlib-command*      "ranlib")
  (set! *mkdir-command*       "mkdir") )

(define (windows-sudo-install-setup)
  (set! *sudo* #f)
  (print "Warning: cannot install as superuser with Windows") )

(define (unix-sudo-install-setup)
  (set! *copy-command*        "sudo cp -r")
  (set! *remove-command*      "sudo rm -fr")
  (set! *move-command*        "sudo mv")
  (set! *chmod-command*       "sudo chmod")
  (set! *ranlib-command*      "sudo ranlib")
  (set! *mkdir-command*       "sudo mkdir") )

(define (user-install-setup)
  (set! *sudo* #f)
  (if *windows-shell*
      (windows-user-install-setup)
      (unix-user-install-setup) ) )

(define (sudo-install-setup)
  (set! *sudo* #t)
  (if *windows-shell*
      (windows-sudo-install-setup)
      (unix-sudo-install-setup) ) )

(define (sudo-install . args)
  (cond ((null? args)   *sudo*)
        ((car args)     (sudo-install-setup))
        (else           (user-install-setup)) ) )

(define abort-setup (make-parameter (cut exit 1)))

(define-syntax ignore-errors
  (syntax-rules ()
    ((_ body ...)
     (handle-exceptions ex #f body ...))))

(define (patch which rx subst)
  (when (setup-verbose-mode) (printf "patching ~A ...~%" which))
  (if (list? which)
      (with-output-to-file (cadr which)
       (lambda ()
	 (with-input-from-file (car which)
	   (lambda ()
	     (let loop ()
	       (let ((ln (read-line)))
		 (unless (eof-object? ln)
		   (write-line (irregex-replace/all rx ln subst))
		   (loop) ) ) ) ) ) ) )
      (let ((tmp (create-temporary-file)))
	(patch (list tmp tmp) rx subst)
	($system 
	 (sprintf "~A ~A ~A" *move-command* (shellpath tmp)
		  (shellpath which))))))

(define run-verbose (make-parameter #t))

(define (register-program name #!optional
			  (path (make-pathname *chicken-bin-path* (->string name))))
  (set! *registered-programs* 
    (alist-cons (->string name) path *registered-programs*)))

(define (find-program name)
  (let* ((name (->string name))
	 (a (assoc name *registered-programs*)))
    (if a
	(shellpath (cdr a))
	name)))

(let ()
  (define (reg name rname) 
    (register-program name (make-pathname *chicken-bin-path* rname)))
  (reg "chicken" (foreign-value "C_CHICKEN_PROGRAM" c-string))
  (reg "csi" (foreign-value "C_CSI_PROGRAM" c-string))
  (reg "csc" (foreign-value "C_CSC_PROGRAM" c-string))
  (reg "chicken-install" (foreign-value "C_CHICKEN_INSTALL_PROGRAM" c-string))
  (reg "chicken-uninstall" (foreign-value "C_CHICKEN_UNINSTALL_PROGRAM" c-string))
  (reg "chicken-status" (foreign-value "C_CHICKEN_STATUS_PROGRAM" c-string))
  (reg "chicken-bug" (foreign-value "C_CHICKEN_BUG_PROGRAM" c-string)))

(define (target-prefix fname)
  (and-let* ((tp (runtime-prefix)))
    (make-pathname tp fname)))

;; Simpler replacement for SRFI-13's string-prefix?
(define (string-prefix? prefix s)
  (let ((pos (substring-index prefix s)))
    (and pos (zero? pos))))

(define (fixpath prg)
  (cond ((string=? prg "csc")
	 (string-intersperse 
	  (cons*
	   (find-program "csc")
	   "-feature" "compiling-extension" 
	   (if (or (deployment-mode)
		   (and (feature? #:cross-chicken)
			(not (host-extension))))
	       "" "-setup-mode")
	   (if (keep-intermediates) "-k" "")
	   (if (host-extension) "-host" "")
	   (if (deployment-mode) "-deployed" "")
	   (append
	    (map (lambda (f)
		   (string-append "-feature " (symbol->string f)))
		 (extra-features))
	    (map (lambda (f)
		   (string-append "-no-feature " (symbol->string f)))
		 (extra-nonfeatures))
	    *csc-options*) )
	  " ") )
	((and (string-prefix? "./" prg) *windows-shell*)
	 (shellpath (substring prg 2)))
	(else (find-program prg))))

(define (execute explist)
  (define (smooth lst)
    (let ((slst (map ->string lst)))
      (string-intersperse (cons (fixpath (car slst)) (cdr slst)) " ") ) )
  (for-each
   (lambda (cmd)
     (when (run-verbose) (printf "  ~A~%~!" cmd))
     ($system cmd))
   (map smooth explist) ) )

(define-syntax run
  (syntax-rules ()
    ((_ exp ...)
     (execute (list `exp ...)))))

(define-syntax compile
  (syntax-rules ()
    ((_ exp ...)
     (run (csc exp ...)))))


;;; Processing setup scripts

(define (make-setup-info-pathname fn #!optional (rpath (repository-path)))
  (make-pathname rpath fn setup-file-extension) )

(define destination-prefix (make-parameter #f))
(define runtime-prefix (make-parameter #f))

(define installation-prefix
  (let ((prefix (get-environment-variable "CHICKEN_INSTALL_PREFIX")))
    (lambda ()
      (or (destination-prefix)
	  prefix
	  chicken-prefix))))

(define create-directory/parents
  (let ()
    (define (verb dir)
      (when (setup-verbose-mode)
	(printf "  mkdir ~a~%~!" dir)) )
    (if *windows*
        (lambda (dir)
          (verb dir)
	  (create-directory dir #t) )
        (lambda (dir)
          (verb dir)
	  (run (,*mkdir-command* -p ,(shellpath dir)) ) ) ) ) )

(define (write-info id files info)
  (let ((info `((files ,@files) 
		,@info)) )
    (when (setup-verbose-mode) (printf "writing info ~A -> ~S ...~%" id info))
    (let* ((sid (->string id))
	   (setup-file (make-setup-info-pathname sid (repo-path #t))))
      (ensure-directory setup-file)
      (cond (*sudo*
	     (let ((tmp (create-temporary-file)))
	       (with-output-to-file tmp (cut pp info))
	       (run (,*move-command* ,(shellpath tmp) ,(shellpath setup-file)))))
	    (else (with-output-to-file setup-file (cut pp info))))
      (unless *windows-shell* (run (,*chmod-command* a+r ,(shellpath setup-file)))))))

(define (copy-file from to #!optional (err #t) (prefix (installation-prefix)))
  ;;XXX the prefix handling is completely bogus
  (let ((from (if (pair? from) (car from) from))
	(to (let ((to-path (if (pair? from) (make-pathname to (cadr from)) to)))
	      (if (not (path-prefix? prefix to-path))
		  (if (absolute-pathname? to-path)
		      to-path
		      (make-pathname prefix to-path) )
		  to-path))))
    (let walk ((from from) (to to))
      (cond ((directory? from)
	     (for-each
	      (lambda (f)
		(walk (make-pathname from f) (make-pathname to f)))
	      (directory from)))
	    (else
	     (ensure-directory to)
	     (run (,*copy-command* 
		   ,(shellpath from)
		   ,(shellpath to))))))
    to))

(define (path-prefix? pref path)
  (string-prefix?
   (normalize-pathname pref)
   (normalize-pathname path)))

(define (move-file from to)
  (let ((from  (if (pair? from) (car from) from))
	(to    (if (pair? from) (make-pathname to (cadr from)) to)))
    (ensure-directory to)
    (run (,*move-command* ,(shellpath from) ,(shellpath to)) ) ) )

(define (remove-file* dir)
  (run (,*remove-command* ,(shellpath dir)) ) )

(define (make-dest-pathname path file)
  (if (list? file)
      (make-dest-pathname path (cadr file))
      (if (absolute-pathname? file)
	  file
	  (make-pathname path file) ) ) )

(define (check-filelist flist)
  (map (lambda (f)
	 (cond ((string? f) f)
	       ((and (list? f) (every string? f)) f)
	       ((and (pair? f) (list (car f) (cdr f))))
	       (else (error "invalid file-specification" f)) ) )
       flist) )

(define (translate-extension f #!optional default)
  (pathname-replace-extension f
   (let ((ext (pathname-extension f)))
     (cond ((not ext) default)
	   ((equal? "so" ext) ##sys#load-dynamic-extension)
	   ((equal? "a" ext) (if *windows-shell* "lib" "a"))
	   (else ext)))))

(define (what-version version)
  (or version
      (let ((n+v (extension-name-and-version)))
	(if (and n+v (pair? n+v) (not (equal? "" (cadr n+v))))
	    (cadr n+v)
	    "unknown"))))

(define (supply-version info version)
  (cond ((assq 'version info) => 
	 (lambda (a)
	   (cons 
	    `(egg-name ,(extension-name))
	    info)))
	(else
	 (let ((v (what-version version)))
	   (cons*
	    `(version ,v)
	    `(egg-name ,(extension-name))
	    info)))))


;;; Convenience function

(define (standard-extension name #!optional version #!key static (info '()))
  ;; `static' is ignored
  (let* ((sname (->string name))
	 (fname (make-pathname #f sname "scm"))
	 (iname (make-pathname #f sname "import.scm"))
	 (ilname (make-pathname #f sname "inline")))
    (compile -dynamic -optimize-level 3 -debug-level 1 ,fname -emit-import-library ,name)
    (compile -dynamic -optimize-level 3 -debug-level 0 ,iname)
    (install-extension
     name
     `(,(pathname-replace-extension fname "so")
       ,(pathname-replace-extension iname "so")
       ,@(if (file-exists? ilname)
	     (list ilname)
	     '()))
     `(,@(supply-version info version)))))


;;; Installation

(define (install-extension id files #!optional (info '()))
  (when (setup-install-mode)
    (let* ((files (check-filelist (if (list? files) files (list files))))
	   (rpath (repo-path))
	   (rpathd (repo-path #t))
	   (dests (map (lambda (f)
			 (let ((from (if (pair? f) (car f) f))
			       (to (make-dest-pathname rpathd f)) )
			   (copy-file from to)
			   (unless *windows-shell*
			     (run (,*chmod-command* a+r ,(shellpath to))))
			   (and-let* ((static (assq 'static info)))
			     (when (and (eq? (software-version) 'macosx)
					(equal? (cadr static) from) 
					(equal? (pathname-extension to) "a"))
				   (run (,*ranlib-command* ,(shellpath to)) ) ))
			   (cond ((deployment-mode) f)
				 ((not (equal? (destination-prefix) (runtime-prefix)))
				  ;; we did not append a prefix already
				  (target-prefix to))
				 ;; There's been destination-prefix added before
				 (else to))))
		       files) ) )
      (write-info id dests (supply-version info #f)) ) ) )

(define (install-program id files #!optional (info '()))
  (define (exify f)
    (translate-extension
     f
     (if *windows-shell* "exe" #f) ) )
  (when (setup-install-mode)
    (let* ((files (check-filelist (if (list? files) files (list files))))
	   (pre (installation-prefix))
	   (ppath (ensure-directory (make-pathname pre "bin") #t))
	   (files (if *windows*
                      (map (lambda (f)
                             (if (list? f) 
                                 (list (exify (car f)) (exify (cadr f)))
                                 (exify f) ) )
                           files)
                      files) ) 
	   (dests (map (lambda (f)
			 (let ((from (if (pair? f) (car f) f))
			       (to (make-dest-pathname ppath f)) )
			   (copy-file from to) 
			   (unless *windows-shell*
				   (run (,*chmod-command* a+r ,(shellpath to))))
			   to) )
		       files) ) )
      (write-info id dests (supply-version info #f)) ) ) )

(define (install-script id files #!optional (info '()))
  (when (setup-install-mode)
    (let* ((files (check-filelist (if (list? files) files (list files))))
	   (pre (installation-prefix))
	   (ppath (ensure-directory (make-pathname pre "bin") #t))
	   (pfiles (map (lambda (f)
			  (let ((from (if (pair? f) (car f) f))
				(to (make-dest-pathname ppath f)) )
			    (copy-file from to) 
			    (unless *windows-shell*
			      (run (,*chmod-command* a+r ,(shellpath to))))
			    to) )
			files) ) )
      (unless *windows-shell*
	(run (,*chmod-command* a+rx ,(string-intersperse pfiles " "))) )
      (write-info id pfiles (supply-version info #f)) ) ) )


;;; More helper stuff

(define (repo-path #!optional ddir?)
  (let ((p (if ddir?
	       (if (deployment-mode)
		   (installation-prefix) ; deploy: copy directly into destdir
		   (let ((p (destination-prefix)))
		     (if p		; installation-prefix changed: use it
			 (make-pathname 
			  p
			  (sprintf "lib/chicken/~a" (##sys#fudge 42)))
			 (repository-path)))) ; otherwise use repo-path
	       (repository-path))) )
    (ensure-directory p #t)
    p) )

(define (ensure-directory path #!optional full)
  (and-let* ((dir (if full path (pathname-directory path))))
    (if (file-exists? dir)
	(unless (directory? dir)
	  (error "cannot create directory: a file with the same name already exists") )
	(begin
	  (create-directory/parents dir)
	  (unless *windows-shell*
	    (run (,*chmod-command* a+x ,(shellpath dir)))))))
  path)

(define (try-compile code #!key c++ (cc (if c++ *cxx* *cc*)) (cflags "") (ldflags "") 
		     (verb (setup-verbose-mode)) (compile-only #f))
  (let* ((fname (create-temporary-file "c"))
	 (oname (pathname-replace-extension fname "o"))
	 (r (begin
	      (with-output-to-file fname (cut display code))
	      (system 
	       (let ((cmd (conc
			   cc " "
			   (if compile-only "-c" "") " "
			   cflags " " *target-cflags* " "
			   (shellpath fname) " -o " (shellpath oname) " "
			   (if compile-only
			       "" 
			       (conc "-L" *target-lib-home* " " ldflags " " *target-libs*) )
			   (if *windows* " >nul: " " >/dev/null ")
			   (if verb "" "2>&1") ) ) )
		 (when verb (print cmd " ..."))
		 cmd) ) ) ) )
    (when verb (print (if (zero? r) "succeeded." "failed.")))
    (ignore-errors ($system (sprintf "~A ~A" *remove-command* (shellpath fname))))
    (ignore-errors ($system (sprintf "~A ~A" *remove-command* (shellpath oname))))
    (zero? r) ) )

(define test-compile try-compile)

(define (find-library name proc)
  (test-compile 
   (sprintf "#ifdef __cplusplus~%extern \"C\"~%#endif~%char ~a();~%int main() { ~a(); return 0; }~%" proc proc)
   ldflags: (conc "-l" name) ) )

(define (find-header name)
  (test-compile
   (sprintf "#include <~a>\nint main() { return 0; }\n" name)
   compile-only: #t) )

(define (version>=? v1 v2)
  (define (version->list v)
    (map (lambda (x) (or (string->number x) x))
	 (irregex-split "[-\\._]" (->string v))))
  (let loop ((p1 (version->list v1))
	     (p2 (version->list v2)))
    (cond ((null? p1) (null? p2))
	  ((null? p2))
	  ((number? (car p1))
	   (and (number? (car p2))
		(or (> (car p1) (car p2))
		    (and (= (car p1) (car p2))
			 (loop (cdr p1) (cdr p2))))))
	  ((number? (car p2)))
	  ((string>? (car p1) (car p2)))
	  (else
	   (and (string=? (car p1) (car p2))
		(loop (cdr p1) (cdr p2)))))))

(define extension-name-and-version
  (make-parameter '("" "")
    (lambda (x)
      (cond [(or (not x) (null? x))
             '("" "") ]
            [(and (list? x) (= 2 (length x)))
             (let ([nam (car x)]
                   [ver (cadr x)]
                   [ensure-string (lambda (x) (if (or (not x) (null? x)) "" (->string x)))])
               (list (ensure-string nam) (ensure-string ver)) ) ]
            [else
             (error "invalid extension-name-and-version" x)]))))

(define (extension-name)
  (car (extension-name-and-version)) )

(define (extension-version #!optional defver)
  (let ([ver (cadr (extension-name-and-version))])
    (if (equal? ver "")
        (and defver (->string defver))
        ver ) ) )

(define (read-info egg #!optional (repo (repository-path)))
  (with-input-from-file 
      (make-pathname repo egg setup-file-extension)
    read))

(define (remove-directory dir #!optional (strict #t))
  (cond ((not (file-exists? dir))
	 (if strict
	     (error 'remove-directory "cannot remove - directory not found" dir)
	     #f))
	(*sudo*
	 (ignore-errors ($system (sprintf "sudo rm -fr ~a" (shellpath dir)))))
	(else
	 (let walk ((dir dir))
	   (let ((files (directory dir #t)))
	     (for-each
	      (lambda (f)
		(unless (or (string=? "." f) (string=? ".." f))
		  (let ((p (make-pathname dir f)))
		    (if (directory? p)
			(walk p) 
			(delete-file p)))))
	      files)
	     (delete-directory dir)))) ))

(define (remove-extension egg #!optional (repo (repository-path)))
  (and-let* ((files (assq 'files (read-info egg repo))))
    (for-each
     (lambda (f)
       (let ((p (if (absolute-pathname? f) f (make-pathname repo f))))
	 (remove-file* p)))
     (cdr files)))
  (remove-file* (make-pathname repo egg setup-file-extension)))

(define ($system str)
  (let ((r (system
	    (if *windows-shell*
		(string-append "\"" str "\"") ; (sic) thanks to Matthew Flatt
		str))))
    (unless (zero? r)
      (error
       (sprintf "shell command failed with nonzero exit status ~a:~%~%  ~a" r str)))))

(define (setup-error-handling)
  (current-exception-handler
   (lambda (c)
     (print-error-message c (current-error-port))
     (reset))))


;;; Module Setup

; User setup by default
(user-install-setup)

)
