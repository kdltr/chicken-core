;;;; reader-tests.scm


(use (only io read-line)
     (only ports with-input-from-string with-output-to-string)
     (only utils read-all))


(set-sharp-read-syntax! #\& (lambda (p) (read p) (values)))
(set-sharp-read-syntax! #\^ (lambda (p) (read p)))
(set-read-syntax! #\! (lambda (p) (read-line p) (values)))

(define output
  (with-output-to-string
    (lambda ()
      (print "hi") ! this is fortran
      (print "foo") #&(print "amp-comment") (print "baz")
      #^(print "bye"))))

!! output:
!! hi
!! foo
!! baz
!! bye

(assert (string=? output "hi\nfoo\nbaz\nbye\n"))
(assert (string=? "   ." (with-input-from-string "\x20\u0020\U00000020\056" read-all)))
