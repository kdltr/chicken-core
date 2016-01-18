(require-extension format)

(define (list-tabulate n proc)
  (let loop ((i 0))
    (if (fx>= i n)
	'()
	(cons (proc i) (loop (fx+ i 1))))))

(define-for-syntax (list-tabulate n proc)
  (let loop ((i 0))
    (if (fx>= i n)
	'()
	(cons (proc i) (loop (fx+ i 1))))))

(define (last lst)
  (let loop ((lst lst))
    (if (null? (cdr lst))
	(car lst)
	(loop (cdr lst)))))

(when (feature? 'manyargs) (print "many arguments supported."))

(define (foo . args)
  (when (pair? args)
    (assert (= (length args) (last args)))))

(printf "testing 'apply' with 0..~A (maximum apply argument count)...\n" 2000)
(do ((i 0 (add1 i)))
    ((>= i 2000))
  (apply foo (list-tabulate i add1)))

(let-syntax
    ((invoke-directly
      (ir-macro-transformer
       (lambda (i r c)
         `(begin
            (print "invoking directly with 0..50...")
            ;; Lowest edge cases
            ,@(list-tabulate 50 (lambda (i) `(foo ,@(list-tabulate i add1))))
            (printf "invoking directly with ~A..~A (maximum ~A direct argument count)...\n"
              ,(- 2000 50) 2000
              (cond-expand (compiling "compiled") (else "interpreted")))
            ;; Highest edge cases
            ,@(list-tabulate
               50 (lambda (i) `(foo ,@(list-tabulate (- 2000 i) add1)))))))))
  (print "If this segfaults on x86-64, try updating GCC (4.5 has a code-generation bug):")
  (invoke-directly))
