;;;; data-structures-tests.scm

(use data-structures)

(define-syntax assert-error
  (syntax-rules ()
    ((_ expr)
     (assert (handle-exceptions _ #t expr #f)))))

(let ((alist '((foo . 123) ("bar" . "baz"))))
  (alist-update! 'foo 999 alist)
  (assert (= (alist-ref 'foo alist) 999))
  (alist-update! 'qux 'nope alist)
  (assert (not (alist-ref 'qux alist)))
  (assert (eq? 'yep (alist-ref 'qux (alist-update! 'qux 'yep alist))))
  (assert (eq? 'ok (alist-ref "bar" (alist-update! "bar" 'ok alist equal?) equal?))))

(let ((alist '((foo . 123) ("bar" . "baz"))))
  (alist-update 'foo 999 alist)
  (assert (= (alist-ref 'foo alist) 123))
  (assert (eq? 'yep (alist-ref 'qux (alist-update 'qux 'yep alist))))
  (assert (eq? 'ok (alist-ref "bar" (alist-update "bar" 'ok alist equal?) equal?))))

;; #808: strings with embedded nul bytes should not be compared
;; with ASCIIZ string comparison functions
(assert (substring=? "foo\x00a" "foo\x00a" 1 1))
(assert (substring-ci=? "foo\x00a" "foo\x00a" 1 1))
(assert (substring-ci=? "foo\x00a" "foo\x00A" 1 1))
(assert (= 2 (substring-index "o\x00bar" "foo\x00bar")))
(assert (= 2 (substring-index-ci "o\x00bar" "foo\x00bar")))
(assert (= 2 (substring-index-ci "o\x00bar" "foo\x00BAR")))
(assert (not (substring=? "foo\x00a" "foo\x00b" 1 1)))
(assert (not (substring-ci=? "foo\x00a" "foo\x00b" 1 1)))
(assert (not (substring-index "o\x00bar" "foo\x00baz")))
(assert (not (substring-index-ci "o\x00bar" "foo\x00baz")))
(assert (= 0 (string-compare3 "foo\x00a" "foo\x00a")))
(assert (> 0 (string-compare3 "foo\x00a" "foo\x00b")))
(assert (< 0 (string-compare3 "foo\x00b" "foo\x00a")))
(assert (= 0 (string-compare3-ci "foo\x00a" "foo\x00a")))
(assert (= 0 (string-compare3-ci "foo\x00a" "foo\x00A")))
(assert (> 0 (string-compare3-ci "foo\x00a" "foo\x00b")))
(assert (> 0 (string-compare3-ci "foo\x00A" "foo\x00b")))
(assert (< 0 (string-compare3-ci "foo\x00b" "foo\x00a")))
(assert (< 0 (string-compare3-ci "foo\x00b" "foo\x00A")))

;; topological-sort

(assert (equal? '() (topological-sort '() eq?)))
(assert (equal? '(a b c d) (topological-sort '((a b) (b c) (c d)) eq?)))
(assert (equal? '(c d a b) (topological-sort '((a b) (c d)) eq?)))
;; (assert-error (topological-sort '((a b) (b a)) eq?))   ;; We don't support cycle detection yet
