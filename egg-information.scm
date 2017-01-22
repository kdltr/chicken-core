;;; loading and accessing egg-information


;;; load egg-info from file and perform validation

(define (load-egg-info fname)
  (with-input-from-file fname read))


;;; lookup specific toplevel properties of egg-information

(define (get-egg-property info prop #!optional default)
  (let ((p (assq prop info)))
    (or (and p (cadr p)) default)))

(define (get-egg-property* info prop #!optional (default '()))
  (let ((p (assq prop info)))
    (or (and p (cdr p)) default)))


;;; lookup specific properties for specific extensions

(define (get-extension-property/internal info ext prop get default host)
  (define (find-prop exp)
    (and (not (null? exp))
         (case (caar exp)
           ((target) 
            (or (and (not host) (find-prop (cdar exp)))
                (find-prop (cdr exp))))
           ((host)
            (or (and host (find-prop (cdar exp)))
                (find-prop (cdr exp))))
           (else (if (eq? prop (caar exp))
                     (car exp)
                     (find-prop (cdr exp)))))))
  (or (let walk ((p (cdr (assq 'components info))))
        (and (not (null? p))
             (case (caar p)
               ((target) 
                (or (and (not host) (walk (cdar p)))
                    (walk (cdr p))))
               ((host)
                (or (and host (walk (cdar p)))
                    (walk (cdr p))))
               ((extension)
                (and (eq? ext (cadar p)) 
                     (let ((p (find-prop (caddr p))))
                       (and p (get p)))))
               (else (walk (cdr p))))))
      default))

(define (get-extension-property info ext prop #!optional default host)
  (get-extension-property/internal info ext prop cadr default host))
  
(define (get-extension-property* info ext prop #!optional default host)
  (get-extension-property/internal info ext prop cdr default host))
