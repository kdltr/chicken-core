;;;; chicken-install.scm
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

(import (scheme))
(import (chicken))
(import (chicken foreign))
(import (chicken data-structures))
(import (chicken keyword))
(import (chicken files))
(import (chicken format))
(import (chicken irregex))
(import (chicken tcp))
(import (chicken ports))
(import (chicken posix))
(import (chicken io))
(import (chicken time))
(import (chicken pretty-print))

(define +defaults-version+ 2)
(define +module-db+ "modules.db")
(define +defaults-file+ "setup.defaults")
(define +short-options+ '(#\r #\h))
(define +one-hour+ (* 60 60))
(define +timestamp-file+ "TIMESTAMP")
(define +status-file+ "STATUS")
(define +egg-extension+ "egg")
(define +egg-info-extension+ "egg.info")

(include "mini-srfi-1.scm")
(include "egg-environment.scm")
(include "egg-information.scm")
(include "egg-compile.scm")
(include "egg-download.scm")

(define user-defaults #f)
(define quiet #t)
(define default-servers '())
(define default-locations '())
(define mappings '())
(define aliases '())
(define override '())
(define hacks '())
(define proxy-host #f)
(define proxy-port #f)
(define proxy-user-pass #f)
(define retrieve-only #f)
(define retrieve-recursive #f)
(define do-not-build #f)
(define list-versions-only #f)
(define canonical-eggs '())
(define dependencies '())
(define checked-eggs '())
(define run-tests #f)
(define force-install #f)
(define host-extension cross-chicken)
(define target-extension cross-chicken)
(define sudo-install #f)
(define update-module-db #f)
(define purge-mode #f)
  
(define platform
  (if (eq? 'mingw (build-platform))
      'windows
      'unix))

(define current-status 
  (list ##sys#build-id default-prefix
        (get-environment-variable "CSC_OPTIONS")
        (get-environment-variable "LD_LIBRARY_PATH")
        (get-environment-variable "DYLD_LIBRARY_PATH")
        (get-environment-variable "CHICKEN_INCLUDE_PATH")
        (get-environment-variable "CHICKEN_REPOSITORY")
        (get-environment-variable "DYLD_LIBRARY_PATH")))

(define (probe-dir dir)
  (and dir (file-exists? dir) (directory? dir) dir))
  
(define cache-directory
  (or (get-environment-variable "CHICKEN_EGG_CACHE")
      (make-pathname (or (probe-dir (get-environment-variable "HOME"))
                         (probe-dir (get-environment-variable "USERPROFILE"))
                         (probe-dir "/tmp")
                         (probe-dir "/Temp")
                         ".")
                     ".chicken-install.cache")))

(define (repo-path)
  (destination-repository
    (if (and cross-chicken (not host-extension))
        'target
        'host)))

(define (build-script-extension mode platform)
  (string-append "build"
                 (if (eq? mode 'target) ".target" "")
                 (if (eq? platform 'windows) ".bat" ".sh")))

(define (install-script-extension mode platform)
  (string-append "install"
                 (if (eq? mode 'target) ".target" "")
                 (if (eq? platform 'windows) ".bat" ".sh")))


;; usage information
  
(define (usage code)
  (print "usage: chicken-install [OPTION | EXTENSION[:VERSION]] ...")
  ;;XXX  
  (exit code))
  

;; utilities

;; Simpler replacement for SRFI-13's string-suffix?
(define (string-suffix? suffix s)
  (let ((len-s (string-length s))
        (len-suffix (string-length suffix)))
     (and (not (< len-s len-suffix))
          (string=? suffix
   	            (substring s (- len-s len-suffix))))))

(define (d flag . args)
  (let ((flag (and (not (string? flag)) flag))
        (fstr (if (string? flag) flag (car args)))
        (args (if (string? flag) args (cdr args))))
    (when (or flag (not quiet))
      (let ((port (current-error-port)))
        (apply fprintf port fstr args)
        (flush-output port) ) )))

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


;; load defaults file ("setup.defaults")

(define (load-defaults)
  (let ((deff (or user-defaults
                  (make-pathname host-sharedir +defaults-file+))))
      (define (broken x)
	(error "invalid entry in defaults file" deff x))
      (cond ((not (file-exists? deff)) '())
            (else
	     (for-each
	      (lambda (x)
		(unless (and (list? x) (positive? (length x)))
		  (broken x))
		(case (car x)
		  ((version)
		   (cond ((not (pair? (cdr x))) (broken x))
			 ((not (= (cadr x) +defaults-version+))
			  (error 
			   (sprintf 
			       "version of installed `~a' does not match chicken-install version (~a)"
			     +defaults-file+
			     +defaults-version+)
			   (cadr x)))
			 ;; others are ignored
			 ))
		  ((server)
		   (set! default-servers
		     (append default-servers (cdr x))))
		  ((map)
		   (set! mappings
		     (append
		      mappings
		      (map (lambda (m)
			     (let ((p (list-index (cut eq? '-> <>) m)))
			       (unless p (broken x))
			       (let-values (((from to) (split-at m p)))
				 (cons from (cdr to)))))
			   (cdr x)))))
		  ((alias)
		   (set! aliases
		     (append 
		      aliases
		      (map (lambda (a)
			     (if (and (list? a) (= 2 (length a)) (every string? a))
				 (cons (car a) (cadr a))
				 (broken x)))
			   (cdr x)))))
		  ((override)
		   (set! override
		     (if (and (pair? (cdr x)) (string? (cadr x)))
			 (call-with-input-file (cadr x) read-all)
			 (cdr x))))
                  ((location)
                   (set! default-locations
                     (append default-locations (list (cdr x)))))
		  ((hack)
		   (set! hacks (append hacks (list (eval (cadr x))))))
		  (else (broken x))))
	      (call-with-input-file deff read-all))))))

  
;; set variables with HTTP proxy information
  
(define (setup-proxy uri)
  (and-let* (((string? uri))
             (m (irregex-match "(http://)?([^:]+):?([0-9]*)" uri))
             (port (irregex-match-substring m 3)))
    (set! proxy-user-pass (get-environment-variable "proxy_auth"))
    (set! proxy-host (irregex-match-substring m 2))
    (set! proxy-port (or (string->number port) 80))))

  
;; apply egg->egg mappings loaded from defaults
  
(define (apply-mappings eggs)
  (define (canonical x)
    (cond ((symbol? x) (cons (symbol->string x) #f))
          ((string? x) (cons x #f))
          ((pair? x) x)
          (else (error "internal error - bad egg spec" x))))
  (define (same? e1 e2)
    (equal? (car (canonical e1)) (car (canonical e2))))
  (let ((eggs2
         (delete-duplicates
           (append-map
             (lambda (egg)
               (cond ((find (lambda (m) (find (cut same? egg <>) (car m)))
                        mappings) => 
                      (lambda (m) (map ->string (cdr m))))
                 (else (list egg))))
             eggs)
           same?)))
    (unless (and (= (length eggs) (length eggs2))
                 (every (lambda (egg) (find (cut same? <> egg) eggs2)) eggs))
      (d "mapped ~s to ~s~%" eggs eggs2))
    eggs2))

  
;; override versions, if specified in "overrides" file
  
(define (override-version egg)
  (let ((name (string->symbol (if (pair? egg) (car egg) egg))))
    (cond ((assq name override) =>
           (lambda (a)
             (cond ((and (pair? egg) (not (equal? (cadr a) (cdr egg))))
                    (warning
                      (sprintf 
                        "version `~a' of extension `~a' overrides explicitly given version `~a'"
                        (cadr a) name (cdr egg))))
                   (else (d "overriding: ~a~%" a)))
             (cadr a)))
          ((pair? egg) (cdr egg))
          (else #f))))
  
  
;; "locate" egg: either perform HTTP download or copy from a file-system 
;; location, also make sure it is up to date
  
(define (locate-egg name version)
  (let* ((cached (make-pathname cache-directory name))
         (now (current-seconds))
         (timestamp (make-pathname cached +timestamp-file+))
         (status (make-pathname cached +status-file+))
         (eggfile (make-pathname cached name +egg-extension+)))
    (define (fetch)
      (when (file-exists? cached)
        (delete-directory cached #t))
      (create-directory cached)
      (fetch-egg-sources name version cached)
      (with-output-to-file status (cut write current-status)))
    (unless (file-exists? cache-directory)
      (create-directory cache-directory))
    (cond ((or (not (probe-dir cached))
               (not (file-exists? eggfile)))
           (d "~a not cached~%" name)
           (fetch))
          ((and (file-exists? status)
                (not (equal? current-status 
                             (with-input-from-file status read))))
           (d "status changed for ~a~%" name)
           (fetch)))
    (let* ((info (load-egg-info eggfile))
           (lversion (get-egg-property info 'version)))
      (cond ((and (file-exists? timestamp)
                  (> (- now (with-input-from-file timestamp read)) +one-hour+)
                  (not (check-remote-version name version 
                                             (and lversion lversion))))
             (d "version of ~a out of date~%" name)
             (fetch)
             (let ((info (load-egg-info eggfile))) ; new egg info (fetched)
               (values cached (get-egg-property info 'version))))
            (else (values cached version))))))
    
(define (resolve-location name)
  (cond ((assoc name aliases) => 
         (lambda (a)
           (let ((new (cdr a)))
             (d "resolving alias `~a' to: ~a~%" name new)
             (resolve-location new))))
        (else name)))

(define (fetch-egg-sources name version dest)
  (let loop ((locs default-locations))
    (cond ((null? locs)
           (let ((tmpdir (create-temporary-directory)))
             (let loop ((srvs default-servers))
               (receive (dir ver)
                 (try-download name (resolve-location (car srvs))
                               version: version 
                               destination: tmpdir
                               tests: run-tests 
                               proxy-host: proxy-host
                               proxy-port: proxy-port 
                               proxy-user-pass: proxy-user-pass)
                 (cond (dir
                         (copy-egg-sources tmpdir dest)
                         (delete-directory tmpdir #t)
                         (with-output-to-file 
                           (make-pathname dest +timestamp-file+)
                           (lambda () (write (current-seconds)))))
                       ((null? srvs) (error "extension or version not found"))
                       (else (loop (cdr srvs))))))))
          ((probe-dir (make-pathname (car locs) name))
           => (lambda (dir)
                (let* ((eggfile (make-pathname dir name +egg-extension+))
                       (info (load-egg-info eggfile))
                       (rversion (get-egg-property info 'version)))
                  (if (or (not rversion)
                          (version>=? rversion version))
                      (copy-egg-sources dir dest)
                      (loop (cdr locs))))))
          (else (loop (cdr locs))))))
  
(define (copy-egg-sources from to)
  ;;XXX should probably be done manually, instead of calling tool
  (let ((cmd (quote-all
               (string-append
                 (copy-directory-command platform)
                 " " (quotearg (make-pathname from "*")) " " (quotearg to))
               platform)))
    (d "~a~%" cmd)
    (system cmd)))
  
(define (check-remote-version name version lversion)
  (let loop ((locs default-locations))
    (cond ((null? locs)
           (let loop ((srvs default-servers))
             (and (pair? srvs)
                  (let ((versions (try-list-versions name (car srvs))))
                    (or (and versions
                             (any (cut version>=? <> version) versions))
                        (loop (cdr srvs)))))))
          ((probe-dir (make-pathname (car locs) name))
           => (lambda (dir)
                (let* ((eggfile (make-pathname dir name +egg-extension+))
                       (info (load-egg-info eggfile))
                       (rversion (get-egg-property info 'version)))
                  (or (and rversion
                           (version>=? rversion version))
                      (loop (cdr locs))))))
          (else (loop (cdr locs))))))


;; retrieve eggs, recursively (if needed)
  
(define (retrieve-eggs eggs)
  (for-each
    (lambda (egg)
      (cond ((assoc egg canonical-eggs) =>
             (lambda (a)
               ;; push to front
               (set! canonical-eggs (cons a (delete a canonical-eggs eq?)))))
            (else
              (let ((name (if (pair? egg) (car egg) egg))
                    (version (override-version egg)))
                (let-values (((dir ver) (locate-egg name version)))
                  (when (or (not dir)
                            (null? (directory dir)))
                    (error "extension or version not found"))
                  (d retrieve-only "~a located at ~a~%" egg dir)
                  (set! canonical-eggs
                    (cons (list name dir ver) canonical-eggs)))))))
     eggs)
  (when (or (not retrieve-only) retrieve-recursive)
    (for-each
      (lambda (e+d+v)
        (unless (member (car e+d+v) checked-eggs)
          (d "checking ~a ...~%" (car e+d+v))
          (set! checked-eggs (cons (car e+d+v) checked-eggs))
          (let* ((fname (make-pathname (cadr e+d+v) (car e+d+v) +egg-extension+))
                 (info (load-egg-info fname)))
            (d "checking platform for `~a'~%" (car e+d+v))
            (check-platform (car e+d+v) info)
            (d "checking dependencies for `~a'~%" (car e+d+v))
            (let-values (((missing upgrade) 
                          (outdated-dependencies (car e+d+v) info)))
              (set! missing (apply-mappings missing))
              (set! dependencies
                (cons (cons (car e+d+v)
                            (map (lambda (mu)
                                   (if (pair? mu)
                                       (car mu)
                                       mu))
                              (append missing upgrade)))
                      dependencies))
              (when (pair? missing)
                (print " missing: " (string-intersperse missing ", "))
                (retrieve-eggs missing))
              (when (and (pair? upgrade)
                         (or force-install
                             (replace-extension-question e+d+v upgrade)))
                (let ((ueggs (unzip1 upgrade)))
                  (d " upgrade: ~a~%" (string-intersperse ueggs ", "))
                  ;; XXX think about this...
                  #;(for-each
                    (lambda (e)
                      (d "removing previously installed extension `~a'" e)
                      (remove-extension e) )
                    ueggs)
                  (retrieve-eggs ueggs) ) ) ) ) ) )
      canonical-eggs)))

(define (outdated-dependencies egg info)
  (let ((ds (get-egg-dependencies info)))
    (for-each
      (lambda (h) (set! ds (h egg ds)))
      hacks)
    (let loop ((deps ds) (missing '()) (upgrade '()))
      (if (null? deps)
          (values (reverse missing) (reverse upgrade))
          (let ((dep (car deps))
                (rest (cdr deps)))
            (let-values (((m u) (check-dependency dep)))
              (loop rest
                    (if m (cons m missing) missing)
                    (if u (cons u upgrade) upgrade))))))))

(define (get-egg-dependencies info)
  (append (get-egg-property* info 'dependencies '())
          (get-egg-property* info 'build-dependencies '())
          (if run-tests (get-egg-property* info 'test-dependencies '()) '())))

(define (check-dependency dep)
  (cond ((or (symbol? dep) (string? dep))
         (values (and (not (ext-version dep)) (->string dep))
                 #f))
        ((and (list? dep) (eq? 'or (car dep)))
         (let scan ((ordeps (cdr dep)) (bestm #f) (bestu #f))
           (if (null? ordeps)
               (values (cond (bestu #f)	; upgrade overrides new
                             (bestm bestm)
                             (else #f))
                       bestu)
               (let-values (((m u) (check-dependency (car ordeps))))
                 (if (and (not m) (not u))
                     (values #f #f)
                     (scan (cdr ordeps)
                           (if (and m (not bestm))
                               m
                               bestm)
                           (if (and u (not bestu))
                               u
                               bestu)))))))
        ((and (list? dep) (= 2 (length dep))
              (or (string? (car dep)) (symbol? (car dep))))
         (let ((v (ext-version (car dep))))
           (cond ((not v)
                  (values (->string (car dep)) #f))
                 ((not (version>=? v (->string (cadr dep))))
                  (cond ((string=? "chicken" (->string (car dep)))
                         (if force-install
                             (values #f #f)
                             (error
                               (string-append 
                                 "Your CHICKEN version is not recent enough to use this extension - version "
                                 (cadr dep) 
				 " or newer is required"))))
                        (else
                          (values #f
                                  (cons (->string (car dep)) (->string (cadr dep)))))))
                 (else (values #f #f)))))
        (else
          (warning "invalid dependency syntax in extension meta information"
                   dep)
          (values #f #f))))

(define (ext-version x)
  (cond ((or (eq? x 'chicken) (equal? x "chicken"))
         (chicken-version))
        ((let* ((ep (##sys#canonicalize-extension-path x 'ext-version))
                (sf (make-pathname (repo-path) ep +egg-info-extension+)))
           (and (file-exists? sf)
                (load-egg-info sf #f))) =>
         (lambda (info)
           (let ((a (assq 'version info)))
             (if a
                 (->string (cadr a))
                 "0.0.0"))))
        (else #f)))

(define (check-platform name info)
  (define (fail)
    (error "extension is not targeted for this system" name))
  (unless cross-chicken
    (and-let* ((platform (get-egg-property info 'platform)))
      (let loop ((p platform))
        (cond ((symbol? p) 
               (or (feature? p) (fail)))
              ((not (list? p))
               (error "invalid `platform' property" name platform))
              ((and (eq? 'not (car p)) (pair? (cdr p)))
               (and (not (loop (cadr p))) (fail)))
              ((eq? 'and (car p))
               (and (every loop (cdr p)) (fail)))
              ((eq? 'or (car p))
               (and (not (any loop (cdr p))) (fail)))
              (else (error "invalid `platform' property" name platform)))))))

(define (replace-extension-question e+d+v upgrade)
  (print (string-intersperse
           (append
             (list "The following installed extensions are outdated, because `"
                   (car e+d+v)
                   "' requires later versions:\n")
                   (filter-map
                     (lambda (e)
                       (cond ((assq (string->symbol (car e)) override) =>
                              (lambda (a)
                                (unless (equal? (cadr a) (cdr e))
                                  (warning
                                    (sprintf "version `~a' of extension `~a' overrides required version `~a'"
                                             (cadr a) (car a) (cdr e))))
                                #f))
                             (else
                               (conc
                                     "  " (car e)
                                     " (" (let ((v (assq 'version (extension-information (car e))))) 
                                            (if v (cadr v) "???"))
                                         " -> " (cdr e) ")"
                                     #\newline) )))
                     upgrade)
                   '("\nDo you want to replace the existing extensions ? (yes/no/abort) "))
            ""))
  (flush-output)
  (let loop ()
    (let ((r (trim (read-line))))
      (cond ((string=? r "yes"))
            ((string=? r "no") #f)
            ((string=? r "abort") (exit 1))
            (else (loop))))))
  
(define (trim str)
  (define (left lst)
    (cond ((null? lst) '())
          ((char-whitespace? (car lst)) (left (cdr lst)))
          (else (cons (car lst) (left (cdr lst))))))
  (list->string (reverse (left (reverse (left (string->list str)))))))
  
  
;; list available egg versions on servers
  
(define (list-egg-versions eggs)
  (let ((srvs (map resolve-location default-servers)))
    (let loop1 ((eggs eggs))
      (unless (null? eggs)
        (let* ((egg (car eggs))
               (name (if (pair? egg) (car egg) egg)))
          (let loop2 ((srvs srvs))
            (and (pair? srvs)
                 (let ((versions (try-list-versions name (car srvs))))
                   (or (and versions 
                            (begin
                              (printf "~a:" name)
                              (for-each (cut printf " ~a" <>) versions)
                              (newline)))
                       (loop2 (cdr srvs))))))
          (loop1 (cdr eggs)))))))

  
;; perform installation of retrieved eggs
  
(define (install-eggs)
  (for-each
    (lambda (egg)
      (let* ((name (car egg))
             (dir (cadr egg))
             (eggfile (make-pathname dir name +egg-extension+))
             (info (load-egg-info eggfile #f)))
        (when (or host-extension 
                  (and (not target-extension)
                       (not host-extension)))
          (let-values (((build install info) (compile-egg-info info platform 'host)))
            (let ((bscript (make-pathname dir name 
                                          (build-script-extension 'host platform)))
                  (iscript (make-pathname dir name 
                                          (install-script-extension 'host
                                                                    platform))))
              (generate-shell-commands platform build bscript dir
                                       (build-prefix 'host name info)
                                       (build-suffix 'host name info))
              (generate-shell-commands platform install iscript dir
                                       (install-prefix 'host name info)
                                       (install-suffix 'host name info))
              (run-script dir bscript platform #f)
              (run-script dir iscript platform sudo-install))))
        (when target-extension
          (let-values (((build install info) (compile-egg-info info platform 'target)))
            (let ((bscript (make-pathname dir name 
                                          (build-script-extension 'target platform)))
                  (iscript (make-pathname dir name 
                                          (install-script-extension 'target 
                                                                    platform))))
              (generate-shell-commands platform build bscript dir
                                       (build-prefix 'target name info)
                                       (build-suffix 'target name info))
              (generate-shell-commands platform install iscript dir
                                       (install-prefix 'target name info)
                                       (install-suffix 'target name info))
              (run-script dir bscript platform #f)
              (run-script dir iscript platform #f))))))
    canonical-eggs))

(define (run-script dir script platform sudo?)
  (if do-not-build
      (print script)
      (let ((old (current-directory)))
        (change-directory dir)
        (d "running script ~a~%" script)
        (if (eq? platform 'windows)
            (exec script)
            (exec (string-append (if sudo? "sudo " "") "sh " script)))
        (change-directory old))))

(define (write-info name info mode)
  (d "writing info for egg ~a~%" name info)
  (let ((infofile (make-pathname name (destination-repository mode))))
    (when (eq? platform 'unix)
      (exec (string-append "chmod a+r " (quotearg infofile))))))

(define (exec cmd)
  (d "executing: ~s~%" cmd)
  (let ((r (system cmd)))
    (unless (zero? r)
      (error "shell command terminated with nonzero exit code" r cmd))))


;;; update module-db

(define (update-db)
  (let* ((files (glob (make-pathname (repo-path) "*.import.so")
                      (make-pathname (repo-path) "*.import.scm")))
         (dbfile (create-temporary-file)))
      (print "loading import libraries ...")
      (fluid-let ((##sys#warnings-enabled #f))
        (for-each
         (lambda (path)
           (let* ((file (pathname-strip-directory path))
		  (import-name (pathname-strip-extension file))
		  (module-name (pathname-strip-extension import-name)))
	     (handle-exceptions ex
		 (print-error-message 
		  ex (current-error-port) 
		  (sprintf "Failed to import from `~a'" file))
	       (eval `(import-syntax ,(string->symbol module-name))))))
         files))
      (print "generating database")
      (let ((db
             (sort
              (append-map
               (lambda (m)
                 (let* ((mod (cdr m))
                        (mname (##sys#module-name mod)))
                   (print* " " mname)
                   (let-values (((_ ve se) (##sys#module-exports mod)))
                     (append
                      (map (lambda (se) (list (car se) 'syntax mname)) se)
                      (map (lambda (ve) (list (car ve) 'value mname)) ve)))))
               ##sys#module-table)
              (lambda (e1 e2)
                (string<? (symbol->string (car e1)) (symbol->string (car e2)))))))
        (newline)
        (with-output-to-file dbfile
          (lambda ()
            (for-each (lambda (x) (write x) (newline)) db)))
        (file-copy dbfile (make-pathname (repo-path) +module-db+) #t))))


;; purge cache for given (or all) eggs

(define (purge-cache eggs)
  (cond ((null? eggs)
         (d "purging complete cache at ~a~%" cache-directory)
         (delete-directory cache-directory #t))
        (else
          (for-each
            (lambda (egg)
              (let* ((name (if (pair? egg) (car egg) egg))
                     (dname (make-pathname cache-directory name)))
                (when (file-exists? dname)
                  (d "purging ~a from cache at ~a~%" name dname)
                  (delete-directory dname #t))))
            eggs))))


;; command line parsing and selection of operations
  
(define (perform-actions eggs)
  (load-defaults)
  (cond (update-module-db (update-db))
        (purge-mode (purge-cache eggs))
        ((null? eggs)
         (cond (list-versions-only (print "no eggs specified"))
               (else
                 (set! canonical-eggs 
                   (map (lambda (fname)
                          (list (pathname-file fname) (current-directory) #f))
                     (glob "*.egg")))
                 (retrieve-eggs '())
                 (unless retrieve-only (install-eggs)))))
        (else
          (let ((eggs (apply-mappings eggs)))
            (cond (list-versions-only (list-egg-versions eggs))
                  ;;XXX other actions...
                  (else 
                    (retrieve-eggs eggs)
                    (unless retrieve-only (install-eggs))))))))

(define (main args)
  (setup-proxy (get-environment-variable "http_proxy"))
  (let ((eggs '())
        (rx (irregex "([^:]+):(.+)")))
    (let loop ((args args))
      (if (null? args)
          (perform-actions (reverse eggs))
          (let ((arg (car args)))
            (cond ((member arg '("-h" "-help" "--help"))
                   (usage 0))
                  ((equal? arg "-test")
                   (set! run-tests #t)
                   (loop (cdr args)))
                  ((equal? arg "-r")
                   (if retrieve-only
                       (set! retrieve-recursive #t)
                       (set! retrieve-only #t))
                   (loop (cdr args)))
                  ((equal? arg "-retrieve")
                   (set! retrieve-only #t)
                   (loop (cdr args)))
                  ((equal? arg "-recursive")
                   (set! retrieve-recursive #t)
                   (loop (cdr args)))
                  ((equal? arg "-list-versions")
                   (set! list-versions-only #t)
                   (loop (cdr args)))
                  ((equal? arg "-defaults")
                   (set! user-defaults (cadr args))
                   (loop (cddr args)))
                  ((equal? arg "-force")
                   (set! force-install #t)
                   (loop (cdr args)))
                  ((equal? arg "-host")
                   (set! target-extension #f)
                   (loop (cdr args)))
                  ((equal? arg "-target")
                   (set! host-extension #f)
                   (loop (cdr args)))
                  ((equal? arg "-update-db")
                   (set! update-module-db #t)
                   (loop (cdr args)))
                  ((equal? arg "-n")
                   (set! do-not-build #t)
                   (loop (cdr args)))
                  ((equal? arg "-v")
                   (set! quiet #f)
                   (loop (cdr args)))
                  ((member arg '("-s" "-sudo"))
                   (set! sudo-install #t)
                   (loop (cdr args)))
                  ((equal? arg "-purge")
                   (set! purge-mode #t)
                   (loop (cdr args)))

                  ;;XXX 
                  
                  ((and (positive? (string-length arg))
                        (char=? #\- (string-ref arg 0)))
                   (if (> (string-length arg) 2)
                       (let ((sos (string->list (substring arg 1))))
                         (if (every (cut memq <> +short-options+) sos)
                             (loop (append 
                                     (map (cut string #\- <>) sos)
                                     (cdr args)))
                             (usage 1)))
                       (usage 1)))
                  ((irregex-match rx arg) =>
                   (lambda (m)
                     (set! eggs
                       (alist-cons
                         (irregex-match-substring m 1)
                         (irregex-match-substring m 2)
                         eggs))
                     (loop (cdr args))))
                  (else 
                    (set! eggs (cons arg eggs))
                    (loop (cdr args)))))))))

(main (command-line-arguments))
  
)
