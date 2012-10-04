;;;; specialization-test-1.scm


(module foo (foo bar)
(import scheme chicken foreign)

#>
static int inlined(int i) {
static int n = 0;
n += i;
return n;}
<#

(: foo (fixnum -> fixnum))

(define (foo i)
  (print "foo: " i)
  0)

(: bar (number -> fixnum)
   ((fixnum) (##core#inline "inlined" #(1))))

(define (bar i)
  (print "bar: " i)
  0)

(assert (zero? (foo 1)))
(assert (zero? (bar 1.0)))
(assert (= 1 (bar 1)))

(: spec (* -> *))
(define (spec x) x)

(define-specialization (spec (x fixnum)) fixnum
  (+ x 1))

(assert (= 2 (spec 1)))

;; "smash-component-types!" had to convert "list[-of]" types to "pair" (#803)
(let ((x (list 'a)))
  (set-cdr! x x)
  (assert (not (list? x))))

;(define (some-proc x y) (if (string->number y) (set-cdr! x x) x))
;(assert (null? (some-proc (list) "invalid number syntax")))

(assert (null? (the (or undefined *) (list))))


)
