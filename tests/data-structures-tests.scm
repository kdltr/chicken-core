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

;; Queues.

;; These are tested extensively (and probably still not enough)
;; because of the strange dealings with the front and end lists stored
;; internally.  If we run into errors, add more regression tests here.

(let ((q (make-queue)))
  (assert (queue? q))
  (assert (queue-empty? q))
  (assert (= 0 (queue-length q)))
  (assert (null? (queue->list q)))
  (assert-error (queue-first q))
  (assert-error (queue-last q))
  (assert-error (queue-remove! q))

  (queue-add! q 'foo)
  (assert (eq? 'foo (queue-first q)))
  (assert (eq? 'foo (queue-last q)))
  (assert (not (queue-empty? q)))
  (assert (= (queue-length q) 1))
  (let ((l1 (queue->list q))
        (l2 (queue->list q)))
    (assert (equal? l1 '(foo)))
    (assert (equal? l2 '(foo)))
    (assert (not (eq? l1 l2)))          ; Do not share memory

    (queue-add! q 'end)

    (queue-push-back! q 'front)

    (assert (equal? l1 '(foo))))      ; Does not share memory w/ queue
  (assert (equal? (queue->list q) '(front foo end)))

  (assert (eq? 'front (queue-remove! q)))
  (assert (eq? 'foo (queue-first q)))
  (assert (eq? 'end (queue-last q)))

  (queue-push-back-list! q '(one two))
  (assert (equal? (queue->list q) '(one two foo end)))
  (assert (= 4 (queue-length q)))

  (assert (eq? 'one (queue-remove! q)))
  (assert (eq? 'two (queue-remove! q)))
  (assert (= 2 (queue-length q)))
  (assert (eq? 'foo (queue-first q)))
  (assert (eq? 'end (queue-last q)))
  (assert (not (queue-empty? q)))

  (assert (eq? 'foo (queue-remove! q)))
  (assert (eq? 'end (queue-first q)))
  (assert (eq? 'end (queue-last q)))
  (assert (= (queue-length q) 1))
  (assert (not (queue-empty? q)))

  (assert (eq? 'end (queue-remove! q)))
  (assert (queue-empty? q))
  (assert (= (queue-length q) 0))
  (assert-error (queue-first q))
  (assert-error (queue-last q))
  (assert-error (queue-remove! q)))

(let ((q (list->queue (list 'one 'two))))
  (assert (queue? q))
  (assert (not (queue-empty? q)))
  (assert (= (queue-length q) 2))
  (assert (eq? 'one (queue-first q)))
  (assert (eq? 'two (queue-last q)))

  (assert (eq? 'one (queue-remove! q)))
  (assert (eq? 'two (queue-first q)))
  (assert (eq? 'two (queue-last q)))
  (assert (= (queue-length q) 1))
  (assert (not (queue-empty? q)))

  (assert (eq? 'two (queue-remove! q)))
  (assert-error (queue-first q))
  (assert-error (queue-last q))
  (assert (= (queue-length q) 0))
  (assert (queue-empty? q)))

(let ((q (list->queue (list 'one))))
  (assert (queue? q))
  (assert (not (queue-empty? q)))
  (assert (= (queue-length q) 1))
  (assert (eq? 'one (queue-first q)))
  (assert (eq? 'one (queue-last q)))

  (queue-push-back! q 'zero)
  (assert (eq? 'zero (queue-first q)))
  (assert (eq? 'one (queue-last q)))

  (queue-add! q 'two)
  (assert (eq? 'zero (queue-first q)))
  (assert (eq? 'two (queue-last q)))

  (queue-add! q 'three)
  (assert (eq? 'zero (queue-first q)))
  (assert (eq? 'three (queue-last q)))
  (assert (equal? '(zero one two three) (queue->list q)))

  (assert (eq? 'zero (queue-remove! q)))
  (assert (eq? 'one (queue-first q)))
  (assert (eq? 'three (queue-last q)))
  (assert (= (queue-length q) 3))
  (assert (not (queue-empty? q)))

  (assert (eq? 'one (queue-remove! q)))
  (assert (eq? 'two (queue-remove! q)))
  (assert (eq? 'three (queue-remove! q)))
  (assert-error (queue-first q))
  (assert-error (queue-last q))
  (assert (= (queue-length q) 0))
  (assert (queue-empty? q)))
