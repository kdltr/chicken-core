;;;; symbolgc-tests.scm
;
; - run this with the "-:w" option

(use gc)

(assert (##sys#fudge 15) "please run this test with the `-:w' runtime option")

(define (gcsome #!optional (n 100))
  (do ((i n (sub1 i))) ((zero? i)) (gc #t)))

(gcsome)

(define *count1* (vector-ref (##sys#symbol-table-info) 2))

(print "starting with " *count1* " symbols")

(print "interning 10000 symbols ...")

(do ((i 10000 (sub1 i)))
    ((zero? i))
  (string->symbol (sprintf "%%%~a%%%" i)))

(print "recovering ...")

(let loop ((i 0))
  (let ((n (vector-ref (##sys#symbol-table-info) 2)))
    (print* (- n *count1*) " ")
    (cond ((> i 100)
	   (unless (<= n *count1*)
	     (error "unable to reclaim all symbols")))
	  ((< (- n *count1*) 100)     ; allow some
	   (gc #t)
	   (loop (+ i 1)))
	  (else 
	   (gc #t)
	   (loop 0)))))

(print "\ndone.")
