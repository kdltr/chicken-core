;;; csc interface tests

(import (chicken pathname)
        (chicken process)
        (chicken process-context)
        (chicken string))

(define (abs x) (make-pathname (current-directory) x))
(define (run x . args) (system* (string-intersperse (cons (abs x) args))))
(define (csc . args) (apply run "../csc" "-v" "-compiler ../chicken" "-I.." args))

(csc "null.scm" "-t")
(assert (file-exists? "null.c"))

(csc "null.c" "-c")
(assert (file-exists? "null.o"))

(csc "null.o")
(run "null")
