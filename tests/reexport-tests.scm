;;;; reexport-tests.scm


(module my-r4rs ()
  (import scheme chicken)
  (reexport 
    (except scheme 
      dynamic-wind values call-with-values eval scheme-report-environment
      null-environment interaction-environment)))

(module m1 ()
  (import my-r4rs)
  (display (+ 3 4))
  (newline))

(assert 
 (not
  (handle-exceptions ex #f
    (eval '(module m2 ()
	     (import my-r4rs)
	     (values 123))))))

(define-syntax compound-module
  (syntax-rules ()
    ((_ name imp ...)
     (module name ()
       (import scheme)
       (reexport imp ...)))))

(compound-module 
 big-chicken
 chicken ports files extras pretty-print data-structures)

(require-library extras data-structures)

(module m3 ()
  (import scheme big-chicken)
  (pp (string-intersperse '("abc" "def" "ghi") "-")))

;;; #901 - reexport with "*" export list

(module
 m4
 (foo-m4)
 (import chicken scheme) 
 (define-syntax foo-m4
   (ir-macro-transformer
    (lambda (e i c)
      ''1))))

(module
 m5
 *					; () works here
 (import chicken scheme)
 (reexport m4))

(import m5)
(print (foo-m4))
