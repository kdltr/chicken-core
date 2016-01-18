;;;; sgrep.scm - grepping benchmark


(use io irregex ports utils)


(define big-string
  (read-all (optional (command-line-arguments) "compiler.scm")))

(define-syntax bgrep
  (syntax-rules ()
    ((_ n expr)
     (time
      (do ((i n (fx- i 1)))
	  ((eq? i 0))
	(with-input-from-string big-string
	  (lambda ()
	    (let ((h 0)
		  (c 0))
	      (do ((line (read-line) (read-line)))
		  ((eof-object? line))
		(set! c (fx+ c 1))
		;(when (zero? (fxmod c 500)) (print* "."))
		(when (irregex-search expr line)
		  (set! h (fx+ h 1))))
	      h))))))))

(define-syntax rx1
  (syntax-rules ()
    ((_) "\\((.*), (.*)\\)")))

(define-syntax rx2
  (syntax-rules ()
    ((_) '(: #\( (submatch (* any)) ", " (submatch (* any))))))

(bgrep 1 (rx1))
