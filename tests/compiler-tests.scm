;;;; compiler-tests.scm


(import foreign)


;; test dropping of previous toplevel assignments

(define (foo) (define (bar) 1) (bar 2))	; will trigger error later
(define bar 1)
(define (baz) 2)
(define (foo) 'ok)

(assert (eq? 'ok (foo)))


;; test hiding of unexported toplevel variables

(module foo (bar)
  (import scheme chicken)
  (declare (hide bar))
  (define (bar x) (+ x 1)))

(assert (not (##sys#symbol-has-toplevel-binding? 'foo#bar)))


;;; rev. 12104 (reported by Joerg Wittenberger)
;
; - canonicalization of assignment to location didn't walk expansion recursively

(define test-location
  (let-location ((again bool #f))
    (lambda ()
      ((foreign-lambda*
	   int
	   (((c-pointer bool) again))
	 "*again=1; return(1);")
       (location again))
      again)))

(assert (test-location))


;;; rev. 12188 (reported by Jörg Wittenberger)
;
; - generated init-assignment refers to alias, but alias isn't seen later)

(module
 x
 (bar)
 (import scheme chicken foreign)

 (define (bar n)
  (let-location ((off integer 0))
    (lambda () 
      ((foreign-lambda*
	   void
	   (((c-pointer integer) i))
	 "(*i)++;")
       (location off))
      off)))
)

(import x)
(assert (= 1 ((bar 42))))

;;; rev. 14574 (reported by Peter Bex)
;
; - type specifiers in foreign-lambda in macros are incorrectly renamed
; - variable names and type specifiers in foreign-lambda* and
;    foreign-primitive in macros are incorrectly renamed

(let-syntax ((strlen-macro
              (syntax-rules ()
                ((strlen-macro arg)
                 (print ((foreign-lambda int strlen c-string) arg)))))
             (strlen-macro*
              (syntax-rules ()
                ((strlen-macro* arg)
                 (print ((foreign-lambda* int ((c-string str))
                                          "C_return(strlen(str));") arg)))))
             (strlen-safe-macro
              (syntax-rules ()
                ((strlen-safe-macro arg)
                 (print ((foreign-safe-lambda int strlen c-string) arg)))))
             (strlen-safe-macro*
              (syntax-rules ()
                ((strlen-safe-macro* arg)
                 (print ((foreign-safe-lambda* int ((c-string str))
                                               "C_return(strlen(str));") arg)))))
             (strlen-primitive-macro
              (syntax-rules ()
                ((strlen-primitive-macro* arg)
                 (print ((foreign-primitive int ((c-string str))
                                            "C_return(strlen(str));") arg))))))
  (strlen-macro "hello, world")
  (strlen-macro* "hello, world")
  (strlen-safe-macro "hello, world")
  (strlen-safe-macro* "hello, world")
  (strlen-primitive-macro "hello, world"))

;; Type specifiers and variable names in foreign-lambda in macros
;; are incorrectly renamed in modules, too.
(foreign-declare "void foo(void *abc) { printf(\"hi\\n\"); }")

(module foo ()
  (import chicken scheme foreign) ; "chicken" includes an export for "void"
  
  (let-syntax ((fl
                (syntax-rules ()
                  ((_)
                   (foreign-lambda void foo (c-pointer void)))))
               (fl*
                (syntax-rules ()
                  ((_)
                   (foreign-lambda* void (((c-pointer void) a))
                                    "C_return(a);"))))
               (fp
                (syntax-rules ()
                  ((_)
                   (foreign-primitive void (((c-pointer void) a))
                                      "C_return(a);")))))
    (fl)
    (fl*)
    (fp)))


;; "const" qualifier should have no visible effect in Scheme
(define-syntax generate-external
  (syntax-rules ()
    ((_) (define-external
           (print_foo ((const c-string) foo))
           void
           (assert (string? foo))
           (print foo)))))
(generate-external)
((foreign-safe-lambda* void () 
   "print_foo(\"bar\");"))


;; Unused arguments in foreign callback wrappers are not optimized away (#584)
(module bla (foo)

(import chicken scheme foreign)

(define-external
  (blabla (int a) (c-string b) (int c) (int d) (c-string e) (int f))
  int
  f)

(define (foo) ((foreign-safe-lambda* int () "C_return(blabla(1, \"2\", 3, 4, \"5\", 6));")))

(assert (location blabla))
)

(import bla)
(assert (= (foo) 6))


;;; compiler-syntax for map/for-each must be careful when the
;   operator may have side-effects (currently only lambda exprs and symbols
;   are allowed)

(let ((x #f))
  (define (f1 x) (print* x " "))
  (map f1 '(1 2 3))
  (newline)
  (map (begin (assert (not x)) 
	      (set! x #t)
	      f1)
       '(1 2 3))
  (map (lambda (x) (print* ":" x)) '(1 2 3))
  (newline))

(let ((x #f))
  (define (f1 x) (print* x " "))
  (let-syntax ((f1 (syntax-rules ()
		     ((_ y) 
		      (begin
			(assert (not x))
			(set! x #t)
			f1)))))
    (for-each f1 '(1 2 3))))

(newline)

;; Test safety of ##sys#make-c-string
(handle-exceptions exn (print "Good, unrepresentable C strings cause errors")
                   (print "BUG! We got, without error, length = "
                          ((foreign-lambda* int ((c-string str))
                                            "C_return(strlen(str));")
                           "foo\x00bar")))


;; failed compile-time argument count check (reported by Alan Post)
;; cbb27fe380ff8e45cdf04d812e1ec649bf45ca47

(define (foo)
  (define (bar #!key a)
    (define (baz)
      (bar a: #t))
    baz)
  bar)


;; global-propagation must also invalidate alias to global if global
;; itself is assigned (reported by Sven Hartrumpf)

(define gp-test-global 0)

(define (gp-test)
  (let ((a gp-test-global)
	(b gp-test-global))
    (set! gp-test-global 1)
    (assert (zero? a))
    (assert (zero? b))))

(gp-test)

;; Test that encode-literal/decode-literal use the proper functions
;; to decode number literals.
(assert (equal? '(+inf.0 -inf.0) (list (fp/ 1.0 0.0) (fp/ -1.0 0.0))))

;; Test that encode-literal doesn't drop digits for extreme flonum values.

;; This number is 2^971 * (2^53 - 1), and is the positive "all ones" number for
;; 64-bit flonums with precision 53 and significand/mantissa 10.
;; If we want to support 32-bit flonums or flonums with different precision
;; or significand, we need a cond-expand here or something.
;; Technically, even larger decimal numbers can be represented by flonums.
;; This number can correctly be compared exactly.
(assert (= (* (- (expt 2 flonum-precision) 1)
              (expt 2 (- flonum-maximum-exponent flonum-precision)))
           179769313486231570814527423731704356798070567525844996598917476803157260780028538760589558632766878171540458953514382464234321326889464182768467546703537516986049910576551282076245490090389328944075868508455133942304583236903222948165808559332123348274797826204144723168738177180919299881250404026184124858368.0
           (string->number "179769313486231570814527423731704356798070567525844996598917476803157260780028538760589558632766878171540458953514382464234321326889464182768467546703537516986049910576551282076245490090389328944075868508455133942304583236903222948165808559332123348274797826204144723168738177180919299881250404026184124858368.0")))

;; #955: unsigned-integer64 arg returned magnitude instead of Scheme object.
#+64bit
(assert (= #xAB54A98CEB1F0AD2
           ((foreign-lambda* unsigned-integer64 ((unsigned-integer64 x))
              "C_return(x);")
            #xAB54A98CEB1F0AD2)))
