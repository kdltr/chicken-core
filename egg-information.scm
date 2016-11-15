;;; loading and accessing egg-information


;;; validate egg-information tree

(define (egg-version? v)
  (and (list? v) 
       (pair? v)
       (null? (cdr v))
       (let ((str (->string (car v))))
         (irregex-match '(seq (+ numeric) 
                              (? #\. (+ numeric)
                                 (? #\. (+ numeric))))
                        str))))

(define (optname? x)
  (and (list? x) (pair? x)
       (or (null? (cdr x)) 
           (string? (cadr x))
           (symbol? (cadr x)))))

;; ENTRY = (NAME TOPLEVEL? NESTED? NAMED? [VALIDATOR])
(define egg-info-items
  `((synopsis #t #f #f)
    (author #t #f #f)
    (category #t #f #f)
    (license #t #f #f)
    (version #t #f #f ,egg-version?)
    (dependencies #t #f #f ,list?)
    (test-dependencies #t #f #f ,list?)
    (build-dependencies #t #f #f ,list?)
    (components #t #f #f)
    (foreign-dependencies #t #f #f ,list?)
    (platform #t #f #f)
    (doc-from-wiki #t #f #f)
    (installed-files #t #f #f ,list?)
    (maintainer #t #f #f)
    (files #f #t #f ,list?)
    (source #f #f #f)
    (csc-options #f #f #f)
    (link-options #f #f #f)
    (custom-build #f #f #f)
    (target #f #t #f)
    (host #f #t #f)
    (types-file #f #f ,optname?)
    (inline-file #f #f ,optname?)
    (extension #f #t #t)
    (generated-source-file #f #t #t)
    (program #f #t #t)
    (data #f #t #t)
    (c-include #f #f #t)
    (scheme-include #f #f #t)))

(define (validate-egg-info info)
  (define (validate info top?)
    (for-each
      (lambda (item)
        (cond ((or (not (pair? item)) 
                   (not (list? item)) 
                   (not (symbol? (car item))))
               (error "invalid egg information item" item))
              ((assq (car item) egg-info-items) =>
               (lambda (a)
                 (apply (lambda (_ toplevel nested named #!optional validator)
                          (when (and top? (not toplevel))
                            (error "egg information item not allowed at toplevel" 
                                   item))
                          (when (and named
                                     (or (null? (cddr item))
                                         (not (symbol? (caddr item)))))
                            (error "unnamed egg information item" item))
                          (when (and validator
                                     (not (validator (cdr item))))
                            (error "egg information item has invalid structure" item))
                          (when nested
                            (validate (if named (cddr item) (cdr item)) #f)))
                        a)))
              (else (error "unknown egg information item" item))))
      info))
  (validate info #t)
  info)


;;; load egg-info from file and perform validation

(define (load-egg-info fname #!optional (validate #t))
  (with-input-from-file fname
    (lambda () 
      (let ((info (read)))
        (if validate
            (validate-egg-info info)
            info)))))


;;; lookup specific entries in egg-information

(define (get-egg-property info prop #!optional default)
  (let ((p (assq prop info)))
    (or (and p (cadr p)) default)))

(define (get-egg-property* info prop #!optional (default '()))
  (let ((p (assq prop info)))
    (or (and p (cdr p)) default)))
