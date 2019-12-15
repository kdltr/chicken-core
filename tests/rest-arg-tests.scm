;; Test rest argument optimizations

;; Check that rest args are correctly fetched from a closure
(assert (equal? 1 ((lambda f0
		     (let ((v0 f0))
		       (let ((failure0
			      (lambda ()
				(if (pair? v0)
				    (car v0)))))
			 (failure0))))
		   1)))

;; Check that rest arg optimizations aren't applied after inlining
;; (#1658), slightly different from the above
(assert (equal? 1 ((lambda f0
		     (let ((v0 f0))
		       (if (pair? v0)
			   (car v0))))
		   1)))

;; Ensure that rest conversion is not applied too aggressively.
;; (only when the consequence is () should it be applied)
(define (rest-nonnull-optimization . rest)
  (let ((x (if (null? (cdr rest))
               '(foo)
               (cdr rest))))
    (null? x)))

(assert (not (rest-nonnull-optimization 1)))
(assert (not (rest-nonnull-optimization 1 2)))

