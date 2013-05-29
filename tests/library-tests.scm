;;;; library-tests.scm

(use srfi-1 extras)

(define-syntax assert-fail
  (syntax-rules ()
    ((_ exp)
     (assert (handle-exceptions ex #t exp #f)))))

;; numbers

(assert (not (not 3)))
(assert (= -4.0 (round -4.3)))
(assert (= -4.0 (round -4.5)))          ; R5RS
(assert (= 4.0 (round 3.5)))
(assert (= 4.0 (round 4.5)))            ; R5RS
(assert (= 4 (round (string->number "7/2"))))
(assert (= 7 (round 7)))
(assert (zero? (round -0.5))) 		; is actually -0.0
(assert (zero? (round -0.3)))
(assert (= -1 (round -0.6)))
(assert (zero? (round 0.5)))
(assert (zero? (round 0.3)))
(assert (= 1.0 (round 0.6)))
(assert (rational? 1))
(assert (finite? 1))
(assert-fail (finite? 'foo))
(assert (rational? 1.0))
(assert (finite? 1.0))
(assert (not (rational? +inf.0)))
(assert (not (finite? +inf.0)))
(assert (not (rational? -inf.0)))
(assert (not (finite? -inf.0)))
(assert (not (rational? +nan.0)))
(assert (not (finite? +nan.0)))
(assert (not (rational? 'foo)))
(assert (not (rational? "foo")))
(assert (integer? 2))
(assert (integer? 2.0))
(assert (not (integer? 1.1)))
(assert (not (integer? +inf.0)))
(assert (not (integer? -inf.0)))
(assert (not (integer? +nan.0)))
(assert (not (integer? 'foo)))
(assert (not (integer? "foo")))
; XXX number missing

(assert (exact? 1))
(assert (not (exact? 1.0)))
(assert (not (exact? 1.1)))
(assert-fail (exact? 'foo))
(assert (not (inexact? 1)))
(assert (inexact? 1.0))
(assert (inexact? 1.1))
(assert-fail (inexact? 'foo))

(assert-fail (/ 1 1 0))
(assert-fail (/ 1 1 0.0))
(assert-fail (/ 1 0.0))
(assert-fail (/ 1 0))
(assert-fail (/ 0))
(assert-fail (/ 0.0))

(assert (fixnum? (/ 1)))

(assert (= -3 (- 3)))
(assert (= 3 (- -3)))
(assert (= 2 (- 5 3)))
(assert (> 1 (/ 3)))
(assert (> 1 (/ 3.0)))
(assert (= 2 (/ 8 4)))
(assert (zero? (+)))
(assert (= 1 (*)))

(assert (= 2.5 (/ 5 2)))

(assert (even? 2))
(assert (even? 2.0))
(assert (even? 0))
(assert (even? 0.0))
(assert (not (even? 3)))
(assert (not (even? 3.0)))
(assert (odd? 1))
(assert (odd? 1.0))
(assert (not (odd? 0)))
(assert (not (odd? 0.0)))
(assert (not (odd? 2)))
(assert (not (odd? 2.0)))
(assert-fail (even? 1.2))
(assert-fail (odd? 1.2))
(assert-fail (even? +inf.0))
(assert-fail (odd? +inf.0))
(assert-fail (even? +nan.0))
(assert-fail (odd? +nan.0))
(assert-fail (even? 'x))
(assert-fail (odd? 'x))

(assert (= 60 (arithmetic-shift 15 2)))
(assert (= 3 (arithmetic-shift 15 -2)))
(assert (= -60 (arithmetic-shift -15 2)))
(assert (= -4 (arithmetic-shift -15 -2))) ; 2's complement
(assert-fail (arithmetic-shift 0.1 2))
;; XXX Do the following two need to fail?  Might as well use the integral value
(assert-fail (arithmetic-shift #xf 2.0))
(assert-fail (arithmetic-shift #xf -2.0))
(assert-fail (arithmetic-shift #xf 2.1))
(assert-fail (arithmetic-shift #xf -2.1))
(assert-fail (arithmetic-shift +inf.0 2))
(assert-fail (arithmetic-shift +nan.0 2))

(assert (= 0 (gcd)))
(assert (= 6 (gcd 6)))
(assert (= 2 (gcd 6 8)))
(assert (= 1 (gcd 6 8 5)))
(assert (= 1 (gcd 6 -8 5)))
(assert (= 2.0 (gcd 6.0 8.0)))
(assert-fail (gcd 6.1 8.0))
(assert-fail (gcd 6.0 8.1))
(assert-fail (gcd +inf.0))
(assert-fail (gcd +nan.0))
(assert-fail (gcd 6.0 +inf.0))
(assert-fail (gcd +inf.0 6.0))
(assert-fail (gcd +nan.0 6.0))
(assert-fail (gcd 6.0 +nan.0))

(assert (= 1 (lcm)))
(assert (= 6 (lcm 6)))
(assert (= 24 (lcm 6 8)))
(assert (= 120 (lcm 6 8 5)))
(assert (= 24.0 (lcm 6.0 8.0)))
(assert-fail (lcm +inf.0))
(assert-fail (lcm +nan.0))
(assert-fail (lcm 6.1 8.0))
(assert-fail (lcm 6.0 8.1))
(assert-fail (lcm 6.0 +inf.0))
(assert-fail (lcm +inf.0 6.0))
(assert-fail (lcm +nan.0 6.0))
(assert-fail (lcm 6.0 +nan.0))

(assert (= 3 (quotient 13 4)))
(assert (= 3.0 (quotient 13.0 4.0)))
(assert-fail (quotient 13.0 4.1))
(assert-fail (quotient 13.2 4.0))
(assert-fail (quotient +inf.0 4.0))
(assert-fail (quotient +nan.0 4.0))
(assert-fail (quotient 4.0 +inf.0))
(assert-fail (quotient 4.0 +nan.0))

(assert (= 1 (remainder 13 4)))
(assert (= 1.0 (remainder 13.0 4.0)))
(assert-fail (remainder 13.0 4.1))
(assert-fail (remainder 13.2 4.0))
(assert-fail (remainder +inf.0 4.0))
(assert-fail (remainder +nan.0 4.0))
(assert-fail (remainder 4.0 +inf.0))
(assert-fail (remainder 4.0 +nan.0))

(assert (= 1 (modulo 13 4)))
(assert (= 1.0 (modulo 13.0 4.0)))
(assert-fail (modulo 13.0 4.1))
(assert-fail (modulo 13.2 4.0))
(assert-fail (modulo +inf.0 4.0))
(assert-fail (modulo +nan.0 4.0))
(assert-fail (modulo 4.0 +inf.0))
(assert-fail (modulo 4.0 +nan.0))

(assert-fail (min 'x))
(assert-fail (max 'x))
(assert (eq? 1 (min 1 2)))
(assert (eq? 1 (min 2 1)))
(assert (eq? 2 (max 1 2)))
(assert (eq? 2 (max 2 1)))
;; must be flonum
(assert (fp= 1.0 (min 1 2.0)))           
(assert (fp= 1.0 (min 2.0 1)))
(assert (fp= 2.0 (max 2 1.0)))           
(assert (fp= 2.0 (max 1.0 2)))

;; number->string conversion

(for-each
 (lambda (x)
   (let ((number (car x))
	 (radix (cadr x)))
     (assert (eqv? number (string->number (number->string number radix) radix)))))
 '((123 10)
   (123 2)
   (123 8)
   (-123 10)
   (-123 2)
   (-123 8)
   (99.2 10)
   (-99.2 10)))

;; by Christian Kellermann
(assert 
 (equal?
  (map (lambda (n) (number->string 32 n)) (iota 15 2))
  '("100000" "1012" "200" "112" "52" "44" "40" "35" "32" "2A" "28" "26" "24" "22" "20")))


;; string->number conversion

(assert (= 255 (string->number "ff" 16)))
(assert (not (string->number "fg" 16)))


;; fp-math

(define (inexact= a b)
  (< (abs (- 1 (abs (/ a b)))) 1e-10))

(assert (inexact= (sin 42.0) (fpsin 42.0)))
(assert (inexact= (cos 42.0) (fpcos 42.0)))
(assert (inexact= (tan 42.0) (fptan 42.0)))
(assert (inexact= (asin 0.5) (fpasin 0.5)))
(assert (inexact= (acos 0.5) (fpacos 0.5)))
(assert (inexact= (atan 0.5) (fpatan 0.5)))
(assert (inexact= (atan 42.0 1.2) (fpatan2 42.0 1.2)))
(assert (inexact= (atan 42.0 1) (fpatan2 42.0 1.0)))
(assert (inexact= (atan 42 1.0) (fpatan2 42.0 1.0)))
(assert (inexact= (exp 42.0) (fpexp 42.0)))
(assert (inexact= (log 42.0) (fplog 42.0)))
(assert (inexact= (expt 42.0 3.5) (fpexpt 42.0 3.5)))
(assert (inexact= (sqrt 42.0) (fpsqrt 42.0)))
(assert (inexact= 43.0 (fpround 42.5)))
(assert (inexact= -43.0 (fpround -42.5)))
(assert (inexact= 42.0 (fpround 42.2)))
(assert (inexact= 42.0 (fptruncate 42.5)))
(assert (inexact= -42.0 (fptruncate -42.5)))
(assert (inexact= 42.0 (fpfloor 42.2)))
(assert (inexact= -43.0 (fpfloor -42.5)))
(assert (inexact= 43.0 (fpceiling 42.5)))
(assert (inexact= -42.0 (fpceiling -42.2)))
(assert (not (fpinteger? 2.3)))
(assert (fpinteger? 1.0))

;;; escaped symbol syntax

(define (rw-invariant? x)   ;; by Jim Ursetto
  (eq? x (with-input-from-string
             (with-output-to-string
               (lambda () (write x)))
           read)))

(assert (string=? "abc" (symbol->string '|abc|)))
(assert (string=? "abcdef" (symbol->string '|abc||def|)))
(assert (string=? "abcxyzdef" (symbol->string '|abc|xyz|def|)))
(assert (string=? "abc|def" (symbol->string '|abc\|def|)))
(assert (string=? "abc|def" (symbol->string '|abc\|def|)))
(assert (string=? "abc" (symbol->string 'abc)))
(assert (string=? "a c" (symbol->string 'a\ c)))
(assert (string=? "aBc" (symbol->string 'aBc)))

(assert (string=? "a\\b" (symbol->string '|a\\b|)))
(assert (rw-invariant? '|a\\b|))
(assert (string=? "3" (symbol->string '|3|)))
(assert (rw-invariant? '|3|))

(parameterize ((case-sensitive #f))
  (assert (string=? "abc" (symbol->string (with-input-from-string "aBc" read))))
  (assert (string=? "aBc" (symbol->string (with-input-from-string "|aBc|" read))))
  (assert (string=? "aBc" (symbol->string (with-input-from-string "a\\Bc" read)))))


;;; keywords

(parameterize ((keyword-style #:suffix))
  (assert (string=? "abc:" (symbol->string (with-input-from-string "|abc:|" read))))
  (assert (string=? "abc" (symbol->string (with-input-from-string "|abc|:" read))))) ; keyword

(parameterize ((keyword-style #:prefix))
  (assert (string=? "abc" (symbol->string (with-input-from-string ":|abc|" read))))
  (assert (string=? ":abc" (symbol->string (with-input-from-string "|:abc|" read)))))

(assert (eq? '|#:| (string->symbol "#:")))
(assert-fail (with-input-from-string "#:" read)) ; empty keyword
(assert (eq? '|#:| (with-input-from-string (with-output-to-string (cut write '|#:|)) read)))

(parameterize ((keyword-style #:suffix))
  (assert (keyword? (with-input-from-string "abc:" read)))
  (assert (keyword? (with-input-from-string "|abc|:" read)))
  (assert (not (keyword? (with-input-from-string "abc:||" read))))
  (assert (not (keyword? (with-input-from-string "abc\\:" read))))
  (assert (not (keyword? (with-input-from-string "abc|:|" read))))
  (assert (not (keyword? (with-input-from-string "|abc:|" read)))))

(parameterize ((keyword-style #:prefix))
  (assert (keyword? (with-input-from-string ":abc" read)))
  (assert (keyword? (with-input-from-string ":|abc|" read)))
  (assert (keyword? (with-input-from-string "||:abc" read))) ;XXX should be not
  (assert (not (keyword? (with-input-from-string "\\:abc" read))))
  (assert (not (keyword? (with-input-from-string "|:|abc" read))))
  (assert (not (keyword? (with-input-from-string "|:abc|" read)))))

(parameterize ((keyword-style #f))
  (assert (not (keyword? (with-input-from-string ":abc" read))))
  (assert (not (keyword? (with-input-from-string ":abc:" read))))
  (assert (not (keyword? (with-input-from-string "abc:" read)))))

(assert (string=? ":" (symbol->string (with-input-from-string ":" read))))
(assert (string=? ":" (symbol->string (with-input-from-string ":||" read))))
(assert-fail (with-input-from-string "#:" read))

(assert (keyword? (with-input-from-string "42:" read)))
(assert (keyword? (with-input-from-string ".:" read)))

(assert (equal? (cons 1 2) (with-input-from-string "(1 . 2)" read)))
(assert (every keyword? (with-input-from-string "(42: abc: .: #:: ::)" read)))


;;; setters

(define x '(a b c))
(define kar car)
(set! (kar (cdr x)) 99)
(assert (equal? '(a 99 c) x))
(define p (make-parameter 100))
(assert (= 100 (p)))
(set! (p) 1000)
(assert (= 1000 (p)))


;;; blob-literal syntax

(assert (equal? '#${a} '#${0a}))
(assert (equal? '#${ab cd} '#${abcd}))
(assert (equal? '#${ab c} '#${ab0c}))
(assert (equal? '#${abc} '#${ab0c}))
(assert (equal? '#${a b c} '#${0a0b0c}))

;; #808: blobs and strings with embedded nul bytes should not be compared
;; with ASCIIZ string comparison functions
(assert (equal? '#${a b 0 c} '#${a b 0 c}))
(assert (blob=? '#${a b 0 c} '#${a b 0 c}))
(assert (equal=? "foo\x00a" "foo\x00a"))
(assert (string=? "foo\x00a" "foo\x00a"))
(assert (string-ci=? "foo\x00a" "foo\x00a"))
(assert (string-ci=? "foo\x00a" "foo\x00A"))
(assert (not (equal? '#${a b 0 c} '#${a b 0 d})))
(assert (not (blob=? '#${a b 0 c} '#${a b 0 d})))
(assert (not (equal=? "foo\x00a" "foo\x00b")))
(assert (not (string=? "foo\x00a" "foo\x00b")))
(assert (not (string-ci=? "foo\x00a" "foo\x00b")))
(assert (string<? "foo\x00a" "foo\x00b"))
(assert (string>? "foo\x00b" "foo\x00a"))
(assert (string-ci<? "foo\x00a" "foo\x00B"))
(assert (string-ci>? "foo\x00b" "foo\x00A"))

;;; getter-with-setter

(define foo
  (let ((m 2))
    (getter-with-setter
     (lambda (x) (* x m))
     (lambda (x) 
       (set! m x)))))

(assert (= 6 (foo 3)))
(set! (foo) 4)
(assert (= 20 (foo 5)))

(define bar
  (getter-with-setter
   foo
   (lambda (x)
     (+ x 99))))

(assert (= 12 (bar 3)))
(assert (= 100 (set! (bar) 1)))
(assert (= 12 (foo 3)))


;;; equal=?

(assert (not (equal=? 1 2)))
(assert (equal=? 1 1))
(assert (equal=? 1 1.0))
(assert (not (equal=? 1 1.2)))
(assert (equal=? 1.0 1))
(assert (equal=? '#(1) '#(1.0)))
(assert (not (equal=? 'a "a")))
(assert (equal=? "abc" "abc"))
(assert (equal=? '(1 2.0 3) '(1 2 3)))
(assert (equal=? '#(1 2.0 3) '#(1 2 3)))
(assert (equal=? '#(1 2 (3)) '#(1 2 (3))))
(assert (not (equal=? '#(1 2 (4)) '#(1 2 (3)))))
(assert (not (equal=? 123 '(123))))

;;; parameters

(define guard-called 0)

(define p
  (make-parameter 
   1
   (lambda (x)
     (set! guard-called (+ guard-called 1))
     x)))

(define k
  (parameterize ((p 2))
    (call/cc
     (lambda (k) 
       (assert (= 2 (p)))
       k))))

(and k (k #f))

(assert (= 2 guard-called))

;;; vector and blob limits

(assert-fail (make-blob -1))
(assert-fail (make-vector -1))

;;; eval return values

(assert (= 1 (eval 1)))
(assert (eq? '() (receive (eval '(values)))))
(assert (equal? '(1 2 3) (receive (eval '(values 1 2 3)))))

;;; message checks for invalid strings

(assert-fail (##sys#message "123\x00456"))

;;; vector procedures

(assert (equal? '#(2 3) (subvector '#(1 2 3) 1)))
(assert (equal? '#(2)   (subvector '#(1 2 3) 1 2)))
(assert (equal? '#()    (subvector '#(1 2 3) 1 1)))
