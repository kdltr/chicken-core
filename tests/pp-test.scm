;;;; pp-test.scm

(use (only pretty-print pp)
     (only ports with-output-to-string))

(define (pp->string thing)
  (with-output-to-string (cut pp thing)))

(define-syntax test 
  (syntax-rules ()
    ((_ result exp)
     (assert (equal? result exp)))))

(test "\"\\\"foo\\\"\"\n" (pp->string "\"foo\""))
(test "\"\\\\\\\"\"\n" (pp->string "\\\""))
(test "\"\\\\\\\\\\\\\\\"\"\n" (pp->string "\\\\\\\""))
(test "\"\\\"\\\"\\\"\"\n" (pp->string "\"\"\""))
(test "\"\\n\\t\\r\\b\\a\\v\\f\"\n" (pp->string "\n\t\r\b\a\v\f"))
(test "\\" "\\")                        ; XXX?
