(require-extension srfi-1)

(when (feature? 'manyargs) (print "many arguments supported."))

(define (foo . args)
  (when (pair? args)
    (assert (= (length args) (last args)))))

(printf "testing 'apply' with 0..~A (maximum apply argument count)...\n" 2000)
(do ((i 0 (add1 i)))
    ((>= i 2000))
  (apply foo (iota i 1)))

(let-syntax
    ((invoke-directly
      (ir-macro-transformer
       (lambda (i r c)
         `(begin
            (print "invoking directly with 0..50...")
            ;; Lowest edge cases
            ,@(list-tabulate 50 (lambda (i) `(foo ,@(iota i 1))))
            (printf "invoking directly with ~A..~A (maximum ~A direct argument count)...\n"
              ,(- 2000 50) 2000
              (cond-expand (compiling "compiled") (else "interpreted")))
            ;; Highest edge cases
            ,@(list-tabulate
               50 (lambda (i) `(foo ,@(iota (- 2000 i) 1)))))))))
  (print "If this segfaults on x86-64, try updating GCC (4.5 has a code-generation bug):")
  (invoke-directly))
