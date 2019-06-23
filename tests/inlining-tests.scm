;;;; inlining-tests.scm - test inlining


;;; SVN rev. 15495: local assignment did not mark lexical as "captured"

(define (foo n)
  (let ((r #f))
    (for-each
     (lambda (m)
       (case m
	 ((abc) (set! r #t))
	 ((def) (set! r 'ok))
	 ((xyz) (set! r 'yo))))
     n)
    r))

(assert (eq? #t (foo '(abc))))


;;; SVN rev. 15511: multiple assignments didn't make global variable unknown for local inlining

(define (bar)
  (set! foo (lambda () 1)))

(define (foo) 0)
(bar)
(assert (= 1 (foo)))

(import inline-me)
(assert (= 42 (foreign-foo 41)))
