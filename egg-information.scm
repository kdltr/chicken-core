;;; loading and accessing egg-information


(define toplevel-items
  '(synopsis authors category license version dependencies
             test-dependencies build-dependencies components foreign-dependencies 
             platform doc-from-wiki))

(define valid-items
  (append toplevel-items
          '(synopsis authors category license version dependencies files
                     source-file csc-options test-dependencies destination linkage
                     build-dependencies components foreign-dependencies link-options
                     custom-bulild target host platform doc-from-wiki extension 
                     program data)))

(define nested-items 
  '(components target host extension program data))

(define named-items
  '(extension program data c-include scheme-include))


;;; validate egg-information tree

(define (validate-egg-info info)
  (define (valid-item? item)
    (and (list? item) (pair? item) (symbol? (car item))))
  (define (toplevel-item? item)
    (and (valid-item? item) (memq (car item) toplevel-items)))
  (unless (list? info) 
    (error "egg-information has invalid structure"))
  (unless (every toplevel-item? info)
    (error "egg-information is invalid toplevel structure"))
  (for-each
    (lambda (item)
      (unless (valid-item? item)
        (error "egg-information item has invalid structure" item))
      (when (and (memq (car item) named-items) (not (symbol? (cadr item))))
        (error "missing name for item" item))
      (if (memq (car item) nested-items)
          (validate-egg-info (if (memq (car item) named-items)
                                 (cddr item) 
                                 (cdr item)))
          (unless (memq (car item) valid-items)
             (error "invalid item" item))))
    info)
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
