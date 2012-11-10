;;;; scrutiny-tests.scm

(define (a)
  (define (b)
    (define (c)
      (let ((x (+ 3 4)))
	(if x 1 2)))))			; expected boolean but got number in conditional

(define (foo x)
  (if x 				; branches return differing number of results
      (values 1 2)
      (values 1 2 (+ (+ (+ (+  3)))))))

(let ((bar +))
  (bar 3 'a))				; expected number, got symbol

(pp)					; expected 1 argument, got 0

(print (cpu-time))			; expected 1 result, got 2
(print (values))			; expected 1 result, got 0

(let ((x 100))
  (x))					; expected procedure, got fixnum

(print (+ 'a 'b))			; expected 2 numbers, but got symbols

(set! car 33)				; 33 does not match type of car

((values 1 2))				; expected procedure, got fixnum (canonicalizes to 1 result)

; this should *not* signal a warning:
(define (test-values x)
  (define (fail) (error "failed"))
  (if x
      (values 42 43)
      (fail)))

; same case, but nested
(define (test-values2 x y)
  (define (fail) (error "failed"))
  (if x
      (values 42 43)
      (if y (values 99 100) (fail))))

(define (foo)
  (define (bar) (if foo 1))		; should not warn (local)
  (for-each void '(1 2 3))		; should not warn (self-call)
  (if foo 2)				; not in tail position
  (if bar 3))				; should warn

;; noreturn conditional branch enforces "number" on x
(define (foo2 x)
  (if (string? x) (error "foo") (+ x 3))
  (string-append x "abc"))

;; implicit declaration of foo3
(declare (hide foo3))

(define (foo3 x)
  (string-append x "abc"))

(foo3 99)

;; predicate
(define (foo4 x)
  (if (string? x)
      (+ x 1)
      (+ x 2)))				; ok

;; enforcement
(define (foo5 x)
  (string-append x "abc")
  (+ x 3))

;; aliasing
(define (foo6 x)
  (let ((y x))
    (string-append x "abc")
    (+ x 3)))				;XXX (+ y 3) does not work yet

;; user-defined predicate
(: foo7 (* -> boolean : string))
(define (foo7 x) (string? x))

(when (foo7 x)
  (+ x 1))				; will warn about "x" being a string

;; declared procedure types are enforcing
(define-type s2s (string -> symbol))

(: foo8 s2s)
(define (foo8 x) (string->symbol x))
(: foo9 s2s)
(declare (enforce-argument-types foo9))
(define (foo9 x) (string->symbol x))

(define (foo10 x)
  (foo8 x)
  (+ x 1)			; foo8 does not enforce x (no warning)
  (foo9 x)			; + enforces number on x
  (+ x 1))			; foo9 does enforce

;; trigger warnings for incompatible types in "the" forms
(define (foo10 x)
  (string-append (the pair (substring x 0 10))) ; 1
  (the * (values 1 2))				; 1 + 2
  (the * (values))				; 3
  (the fixnum (* x y)))				; nothing (but warns about "x" being string)


;; Reported by Joerg Wittenberger:
;
; - assignment inside first conditional does not invalidate blist
;;  entries for "ins"/"del" in outer flow.

(define (write-blob-to-sql sql identifier last blob c-c)
 (define ins '())
 (define del '())
 (if (vector? blob)
     (begin
	(set! ins (vector-ref blob 1))
	(set! del (vector-ref blob 2))
	(set! blob (vector-ref blob 0))))
 (if (or (pair? ins)
	 (pair? del))
     (<handle-ins-and-del>))
 (<do-some-more>))

;; Checking whether reported line numbers inside modules are correct
(module foo (blabla)
  (import chicken scheme)
  (define (blabla)
    (+ 1 'x)))

;; Reported by megane in #884:
;;
;; Custom types defined in modules need to be resolved during canonicalization
(module bar ()
  (import chicken scheme)
  (define-type footype string)
  (the footype "bar"))

(: deprecated-procedure deprecated)
(define (deprecated-procedure x) (+ x x))
(deprecated-procedure 1)

(: another-deprecated-procedure (deprecated replacement-procedure))
(define (another-deprecated-procedure x) (+ x x))
(another-deprecated-procedure 2)

;; Needed to use "over-all-instantiations" or matching "vector"/"list" type
;; with "vector-of"/"list-of" type (reported by megane)
(: apply1 (forall (a b) (procedure ((procedure (#!rest a) b) (list-of a)) b)))

(define (apply1 f args)
  (apply f args))

(apply1 + (list 'a 2 3)) ; <- no type warning (#948)
(apply1 + (cons 'a (cons 2 (cons 3 '())))) ; <- same here (#952)

