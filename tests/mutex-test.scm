;;;; mutex-test.scm

(require-extension srfi-18)

(define test-has-failed #f)

(define (test-error x . more)
  (set! test-has-failed #t)
  (apply print x more))

(define (test-exit x)
  (set! test-has-failed #t)
  x)

#|  The mutex data structure.

Slot  Type                           Meaning
1     *                              name
2     (or false (struct thread))     owner
3     (list-of (struct thread))      waiting thread
4     boolean                        abandoned
5     boolean                        blocked

|#

(define-record-printer (mutex x out)
  (format out "<mutex ~a ~a~a ~a (owner ~a) waiting ~a>"
	  (mutex-name x)
	  (if (##sys#slot x 5) "LOCKED" "FREE")
	  (if (##sys#slot x 4) "/ABANDONED" "")
	  (mutex-state x)
	  (if (##sys#slot x 2) (##sys#slot x 2) "none")
	  (##sys#slot x 3)
	  ))

(define (dbg l v)
  (format (current-error-port) "D ~a: ~a\n" l v) v)

(define mux1 (make-mutex 'test-lock-fail-with-timeout))

(mutex-lock! mux1)

(define owner1 (mutex-state mux1))

(thread-join!
 (thread-start!
  (lambda ()
    (assert (eq? (mutex-lock! mux1 0.1) #f))
    (when
     (memq (current-thread) (##sys#slot mux1 3))
     (print "Got " mux1 " found this thread still waiting!\n")
     (test-exit 1))
    (when
     (not (eq? (mutex-state mux1) owner1))
     (print "Got " mux1 " state " (mutex-state mux1) " expected " owner1 "\n")
     (test-exit 1)))))

(set! mux1 (make-mutex 'unlock-leaves-no-memory-leak))
(mutex-lock! mux1)
(mutex-unlock! mux1)
(when
 (not (eq? (##sys#slot mux1 2) #f))
 (test-error "thread still held in mutex after unlock: " mux1))

;;============
; Make a locked mutex
(define mux (make-mutex 'foo))
(mutex-lock! mux #f #f)

;; Have a thread waiting for it.

(define t1
  (thread-start!
   (lambda ()
    (mutex-lock! mux #f #f)
    (when (not (eq? (mutex-state mux) 'not-owned))
      (print "Got " mux " state " (mutex-state mux) " expected " 'not-owned "\n")
      (test-exit 1)))))

;; Give it time to actually wait.

(thread-yield!)

;; Let it lock the mux

(mutex-unlock! mux)

(thread-yield!)

(or (eq? (mutex-state mux) 'not-owned)
    (test-error "Expected 'not-owned got " (mutex-state mux) mux))

(set! t1
  (thread-start!
   (lambda ()
    (mutex-lock! mux)
    (when (not (eq? (mutex-state mux) (current-thread)))
      (print "Got " mux " state " (mutex-state mux) " expected " (current-thread) "\n")
      (test-exit 1)))))

(mutex-unlock! mux)

(thread-yield!)

;; check that it is properly abandoned

(when (not (handle-exceptions ex (abandoned-mutex-exception? ex) (and (mutex-lock! mux #f) #f)))
  (print "Abandoned Mutex not abandoned " mux "\n")
  (test-exit 1))

(mutex-unlock! mux)

(mutex-lock! mux)

(when (not (eq? (mutex-state mux) (current-thread)))
  (print "Got " mux " state " (mutex-state mux) " expected " (current-thread) "\n")
  (test-exit 1))

(cond-expand (dribble
(define-for-syntax count 0)
(define-syntax trail
  (lambda (form r c)			; doesn't bother much with renaming
    (let ((loc (cadr form))
	  (expr (caddr form)))
      (set! count (add1 count))
      `(,(r 'begin)
	(print "(" ,count ") " ,loc ": " ',expr ": get: " (##sys#slot get-mutex 5) ", put: " (##sys#slot put-mutex 5))
	(let ((xxx ,expr))
	  (print "  (" ,count ") " ,loc ": " ',expr ": get: " (##sys#slot get-mutex 5) ", put: " (##sys#slot put-mutex 5))
	  xxx) ) ))))
(else (define-syntax trail (syntax-rules () ((_ loc expr) expr)))))

(define (tprint . x)
 (printf "~a " (current-milliseconds))
 (apply print x))

(define (make-empty-mailbox)
 (let ((put-mutex (make-mutex))        ; allow put! operation
       (get-mutex (make-mutex))
       (cell #f))

   (define (put! obj)
     (trail 'put! (mutex-lock! put-mutex #f #f))     ; prevent put! operation
     (set! cell obj)
     (trail 'put! (mutex-unlock! get-mutex)) )

   (define (get!)
     (trail 'get! (mutex-lock! get-mutex #f #f))     ; wait until object in mailbox
     (let ((result cell))
       (set! cell #f)                  ; prevent space leaks
       (trail 'get! (mutex-unlock! put-mutex))       ; allow put! operation
       result))

   (trail 'main (mutex-lock! get-mutex #f #f))       ; prevent get! operation

   (lambda (print)
     (case print
       ((put!) put!)
       ((get!) get!)
       (else (error "unknown message"))))))

(define (mailbox-put! m obj) ((m 'put!) obj))
(define (mailbox-get! m) ((m 'get!)))

;(tprint 'start)

(define mb (make-empty-mailbox))

(thread-start!
 (make-thread
 (lambda ()
   (let lp ()
     ;(print "1: get")
     (let ((x (mailbox-get! mb)))
       ;(tprint "read: " x)
       (assert x)
       (lp))))))

(thread-start!
 (make-thread
 (lambda ()
   (thread-sleep! 1)
   ;(tprint 'put)
   ;(print "2: put")
   (mailbox-put! mb 'test)
   #;(print "2: endput"))))

(thread-sleep! 3)
;(tprint 'exit)

(if test-has-failed (exit 1) (exit 0))
