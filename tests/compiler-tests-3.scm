;;; compiler-tests-3.scm - tests for unboxing

(use flonum)

;;; unboxing introduced binding in test-position of conditional

;;; MBROT -- Generation of Mandelbrot set fractal.

(define (count r i step x y)

  (let ((max-count 64)
        (radius^2  16.0))

    (let ((cr (fp+ r (fp* (exact->inexact x) step)))
          (ci (fp+ i (fp* (exact->inexact y) step))))
      
      (let loop ((zr cr)
                 (zi ci)
                 (c 0))
        (if (= c max-count)
          c
          (let ((zr^2 (fp* zr zr))
                (zi^2 (fp* zi zi)))
            (if (fp> (fp+ zr^2 zi^2) radius^2)
              c
              (let ((new-zr (fp+ (fp- zr^2 zi^2) cr))
                    (new-zi (fp+ (fp* 2.0 (fp* zr zi)) ci)))
                (loop new-zr new-zi (+ c 1))))))))))

(define (mbrot matrix r i step n)
  (let loop1 ((y (- n 1)))
    (if (>= y 0)
      (let loop2 ((x (- n 1)))
        (if (>= x 0)
          (begin
            (vector-set! (vector-ref matrix x) y (count r i step x y))
            (loop2 (- x 1)))
          (loop1 (- y 1)))))))

(define (test n)
  (let ((matrix (make-vector n)))
    (let loop ((i (- n 1)))
      (if (>= i 0)
        (begin
          (vector-set! matrix i (make-vector n))
          (loop (- i 1)))))
    (mbrot matrix -1.0 -0.5 0.005 n)
    (vector-ref (vector-ref matrix 0) 0)))

(define (main . args)
  (let ((r (test 75)))
    (unless (equal? r 5)
    (error "incorrect result: " r))))
