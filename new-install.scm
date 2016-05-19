;;;;

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
(import (chicken posix))
(import (chicken io))
(import (chicken pretty-print))

(define +defaults-version+ 1)
(define +module-db+ "modules.db")
(define +defaults-file+ "setup.defaults")
(define +short-options+ '(#\h))

(include "mini-srfi-1.scm")
(include "egg-environment.scm")
(include "egg-compile.scm")
(include "egg-download.scm")

(define quiet #f)
(define default-sources '())
(define mappings '())
(define aliases '())
(define override '())
(define hacks '())
(define proxy-host #f)
(define proxy-port #f)
(define proxy-user-pass #f)
  
  
;; usage information
  
(define (usage code)
  
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

(define (d fstr . args)
  (let ((port (if quiet (current-error-port) (current-output-port))))
    (apply fprintf port fstr args)
    (flush-output port) ) )


;; load defaults file ("setup.defaults")

(define (load-defaults)
  (let ((deff (make-pathname host-sharedir +defaults-file+)))
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
		   (set! default-sources
		     (append default-sources (list (cdr x)))))
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
		  ((hack)
		   (set! hacks (append hacks (list (eval (cadr x))))))
		  (else (broken x))))
	      (call-with-input-file deff read-all))))
      (pair? default-sources) ))

(define (setup-proxy uri)
  (and-let* (((string? uri))
             (m (irregex-match "(http://)?([^:]+):?([0-9]*)" uri))
             (port (irregex-match-substring m 3)))
    (set! proxy-user-pass (get-environment-variable "proxy_auth"))
    (set! proxy-host (irregex-match-substring m 2))
    (set! proxy-port (or (string->number port) 80))))

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
                        *mappings*) => 
                      (lambda (m) (map ->string (cdr m))))
                 (else (list egg))))
             eggs)
           same?)))
    (unless (and (= (length eggs) (length eggs2))
                 (every (lambda (egg) (find (cut same? <> egg) eggs2)) eggs))
      (print "mapped " eggs " to " eggs2))
    eggs2))

(define (perform-actions eggs)
  (let ((eggs (apply-mappings eggs)))
    

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

                  ;;XXX 
                  
                  ((and (positive? (string-length arg))
                        (char=? #\- (string-ref arg 0)))
                   (if (> (string-length arg) 2)
                       (let ((sos (string->list (substring arg 1))))
                         (if (every (cut memq <> +short-options+) sos)
                             (loop (append 
                                     (map (cut string #\- <>) sos)
                                     (cdr args)) 
                                   eggs)
                             (usage 1)))
                       (usage 1)))
                  ((irregex-match rx arg) =>
                   (lambda (m)
                     (set! eggs
                       (alist-cons
                         (irregex-match-substring m 1)
                         (irregex-match-substring m 2)
                         eggs))))
                  (else 
                    (set! eggs (cons arg args))
                    (loop (cdr args)))))))))

(main (command-line-arguments))
  
)
