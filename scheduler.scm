; scheduler.scm - Basic scheduler for multithreading
;
; Copyright (c) 2008-2013, The Chicken Team
; Copyright (c) 2000-2007, Felix L. Winkelmann
; All rights reserved.
;
; Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following
; conditions are met:
;
;   Redistributions of source code must retain the above copyright notice, this list of conditions and the following
;     disclaimer. 
;   Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following
;     disclaimer in the documentation and/or other materials provided with the distribution. 
;   Neither the name of the author nor the names of its contributors may be used to endorse or promote
;     products derived from this software without specific prior written permission. 
;
; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS
; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
; AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR
; CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
; OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
; POSSIBILITY OF SUCH DAMAGE.


(declare
  (unit scheduler)
  (disable-interrupts)
  (hide ready-queue-head ready-queue-tail ##sys#timeout-list
	##sys#update-thread-state-buffer ##sys#restore-thread-state-buffer
	remove-from-ready-queue ##sys#unblock-threads-for-i/o ##sys#force-primordial
	fdset-set fdset-test create-fdset stderr
	##sys#clear-i/o-state-for-thread! ##sys#abandon-mutexes) 
  (not inline ##sys#interrupt-hook)
  (unsafe)
  (foreign-declare #<<EOF
#ifdef HAVE_ERRNO_H
# include <errno.h>
# define C_signal_interrupted_p     C_mk_bool(errno == EINTR)
#else
# define C_signal_interrupted_p     C_SCHEME_FALSE
#endif

#ifdef _WIN32
# if (defined(HAVE_WINSOCK2_H) && defined(HAVE_WS2TCPIP_H))
#  include <winsock2.h>
#  include <ws2tcpip.h>
# else
#  include <winsock.h>
# endif
/* Beware: winsock2.h must come BEFORE windows.h */
# define C_msleep(n)     (Sleep(C_unfix(n)), C_SCHEME_TRUE)
#else
# include <unistd.h>
# include <sys/types.h>
# include <sys/time.h>
# include <time.h>
static C_word C_msleep(C_word ms);
C_word C_msleep(C_word ms) {
#ifdef __CYGWIN__
  if(usleep(C_unfix(ms) * 1000) == -1) return C_SCHEME_FALSE;
#else
  struct timespec ts;
  unsigned long mss = C_unfix(ms);
  ts.tv_sec = mss / 1000;
  ts.tv_nsec = (mss % 1000) * 1000000;
  
  if(nanosleep(&ts, NULL) == -1) return C_SCHEME_FALSE;
#endif
  return C_SCHEME_TRUE;
}
#endif

#ifdef HAVE_POSIX_POLL
#  include <poll.h>
#  include <assert.h>

static int C_fdset_nfds;
static struct pollfd *C_fdset_set = NULL;

C_inline int C_fd_ready(int fd, int pos, int what) {
  assert(fd == C_fdset_set[pos].fd); /* Must match position in ##sys#fd-list! */
  return(C_fdset_set[pos].revents & what);
}

#define C_fd_input_ready(fd,pos)  C_mk_bool(C_fd_ready(C_unfix(fd), C_unfix(pos),POLLIN|POLLERR|POLLHUP|POLLNVAL))
#define C_fd_output_ready(fd,pos)  C_mk_bool(C_fd_ready(C_unfix(fd), C_unfix(pos),POLLOUT|POLLERR|POLLHUP|POLLNVAL))

C_inline int C_ready_fds_timeout(int to, double tm) {
  return poll(C_fdset_set, C_fdset_nfds, to ? (int)tm : -1);
}

C_inline void C_prepare_fdset(int length) {
  /* TODO: Only realloc when needed? */
  C_fdset_set = realloc(C_fdset_set, sizeof(struct pollfd) * length);
  if (C_fdset_set == NULL)
    C_halt(C_SCHEME_FALSE); /* Ugly: no message */
  C_fdset_nfds = 0;
}

/* This *must* be called in order, so position will match ##sys#fd-list */
C_inline void C_fdset_add(int fd, int input, int output) {
  C_fdset_set[C_fdset_nfds].events = ((input ? POLLIN : 0) | (output ? POLLOUT : 0));
  C_fdset_set[C_fdset_nfds++].fd = fd;
}

#else

/* Shouldn't we include <sys/select.h> here? */
static fd_set C_fdset_input, C_fdset_output;

#define C_fd_input_ready(fd,pos)  C_mk_bool(FD_ISSET(C_unfix(fd), &C_fdset_input))
#define C_fd_output_ready(fd,pos)  C_mk_bool(FD_ISSET(C_unfix(fd), &C_fdset_output))

C_inline int C_ready_fds_timeout(int to, double tm) {
  struct timeval timeout;
  timeout.tv_sec = tm / 1000;
  timeout.tv_usec = fmod(tm, 1000) * 1000;
  /* we use FD_SETSIZE, but really should use max fd */
  return select(FD_SETSIZE, &C_fdset_input, &C_fdset_output, NULL, to ? &timeout : NULL);
}

C_inline void C_prepare_fdset(int length) {
  FD_ZERO(&C_fdset_input);
  FD_ZERO(&C_fdset_output);
}

C_inline void C_fdset_add(int fd, int input, int output) {
  if (input) FD_SET(fd, &C_fdset_input);
  if (output) FD_SET(fd, &C_fdset_output);
}
#endif
EOF
) )

(include "common-declarations.scm")

#;(begin
  (define stderr ##sys#standard-error) ; use default stderr port
  (define (dbg . args)
    (parameterize ((##sys#print-length-limit #f))
      (for-each
       (lambda (x)
	 (display x stderr))
       args)
      (newline stderr))))

(define-syntax dbg
  (syntax-rules ()
    ((_ . _) #f))) 

(define-syntax panic
  (syntax-rules ()
    ((_ msg) (##core#inline "C_halt" msg))))

(define (##sys#schedule)
  (define (switch thread)
    (dbg "switching to " thread)
    (set! ##sys#current-thread thread)
    (##sys#setslot thread 3 'running)
    (##sys#restore-thread-state-buffer thread)
    (##core#inline "C_set_initial_timer_interrupt_period" (##sys#slot thread 9))
    ((##sys#slot thread 1)) )
  (let* ([ct ##sys#current-thread]
	 [eintr #f]
	 [cts (##sys#slot ct 3)] )
    (dbg "==================== scheduling, current: " ct ", ready: " ready-queue-head)
    (##sys#update-thread-state-buffer ct)
    ;; Put current thread on ready-queue:
    (when (or (eq? cts 'running) (eq? cts 'ready)) ; should ct really be 'ready? - normally not.
      (##sys#setislot ct 13 #f)			   ; clear timeout-unblock flag
      (##sys#add-to-ready-queue ct) )
    (let loop1 ()
      ;; Unblock threads waiting for timeout:
      (unless (null? ##sys#timeout-list)
	(let ((now (##core#inline_allocate ("C_a_i_current_milliseconds" 4) #f)))
	  (let loop ((lst ##sys#timeout-list))
	    (if (null? lst)
		(set! ##sys#timeout-list '())
		(let* ([tmo1 (caar lst)] ; timeout of thread on list
		       [tto (cdar lst)]	 ; thread on list
		       [tmo2 (##sys#slot tto 4)] ) ; timeout value stored in thread
		  (dbg "timeout: " tto " -> " tmo2 " (now: " now ")")
		  (if (equal? tmo1 tmo2)  ;XXX why do we check this?
		      (if (fp>= now tmo1) ; timeout reached?
			  (begin
			    (##sys#setislot tto 13 #t) ; mark as being unblocked by timeout
			    (##sys#clear-i/o-state-for-thread! tto)
			    (##sys#thread-basic-unblock! tto)
			    (loop (cdr lst)) )
			  (begin
			    (set! ##sys#timeout-list lst) 
			    ;; If there are no threads blocking on a select call (fd-list)
			    ;; but there are threads in the timeout list then sleep for
			    ;; the number of milliseconds of next thread to wake up.
			    (when (and (null? ready-queue-head)
				       (null? ##sys#fd-list) 
				       (pair? ##sys#timeout-list))
			      (let ((tmo1 (caar ##sys#timeout-list)))
				(set! eintr
				  (and (not (##core#inline 
					     "C_msleep" 
					     (fxmax 
					      0
					      (##core#inline "C_quickflonumtruncate" (fp- tmo1 now)))))
				       (foreign-value
					"C_signal_interrupted_p" bool) ) ) ) ) ) )
		      (loop (cdr lst)) ) ) ) ) ) )
      ;; Unblock threads blocked by I/O:
      (if eintr
	  (##sys#force-primordial)	; force it to handle user-interrupt
	  (unless (null? ##sys#fd-list)
	    (##sys#unblock-threads-for-i/o) ) )
      ;; Fetch and activate next ready thread:
      (let loop2 ()
	(let ([nt (remove-from-ready-queue)])
	  (cond [(not nt) 
		 (if (and (null? ##sys#timeout-list) (null? ##sys#fd-list))
		     (panic "deadlock")
		     (loop1) ) ]
		[(eq? (##sys#slot nt 3) 'ready) (switch nt)]
		[else (loop2)] ) ) ) ) ) )

(define (##sys#force-primordial)
  (dbg "primordial thread forced due to interrupt")
  (##sys#setislot ##sys#primordial-thread 13 #f)
  (##sys#thread-unblock! ##sys#primordial-thread) )

(define ready-queue-head '())
(define ready-queue-tail '())

(define (##sys#ready-queue) ready-queue-head)

(define (##sys#add-to-ready-queue thread)
  (##sys#setslot thread 3 'ready)
  (let ((new-pair (cons thread '())))
    (cond ((eq? '() ready-queue-head) 
	   (set! ready-queue-head new-pair))
	  (else (set-cdr! ready-queue-tail new-pair)) )
    (set! ready-queue-tail new-pair) ) )

(define (remove-from-ready-queue)
  (let ((first-pair ready-queue-head))
    (and (not (null? first-pair))
	 (let ((first-cdr (cdr first-pair)))
	   (set! ready-queue-head first-cdr)
	   (when (eq? '() first-cdr) (set! ready-queue-tail '()))
	   (car first-pair) ) ) ) )

(define (##sys#update-thread-state-buffer thread)
  (let ([buf (##sys#slot thread 5)])
    (##sys#setslot buf 0 ##sys#dynamic-winds)
    (##sys#setslot buf 1 ##sys#standard-input)
    (##sys#setslot buf 2 ##sys#standard-output)
    (##sys#setslot buf 3 ##sys#standard-error)
    (##sys#setslot buf 4 ##sys#current-exception-handler)
    (##sys#setslot buf 5 ##sys#current-parameter-vector) ) )

(define (##sys#restore-thread-state-buffer thread)
  (let ([buf (##sys#slot thread 5)])
    (set! ##sys#dynamic-winds (##sys#slot buf 0))
    (set! ##sys#standard-input (##sys#slot buf 1))
    (set! ##sys#standard-output (##sys#slot buf 2))
    (set! ##sys#standard-error (##sys#slot buf 3)) 
    (set! ##sys#current-exception-handler (##sys#slot buf 4))
    (set! ##sys#current-parameter-vector (##sys#slot buf 5)) ) )

(set! ##sys#interrupt-hook
  (let ([oldhook ##sys#interrupt-hook])
    (lambda (reason state)
      (when (fx= reason 255)		; C_TIMER_INTERRUPT_NUMBER
	(let ([ct ##sys#current-thread])
	  (##sys#setslot ct 1 (lambda () (oldhook reason state))) 
	  (##sys#schedule) ) )		; expected not to return!
      (oldhook reason state) ) ) )

(define ##sys#timeout-list '())

(define (##sys#remove-from-timeout-list t)
  (let loop ((l ##sys#timeout-list) (prev #f))
    (if (null? l)
	l
	(let ((h (##sys#slot l 0))
	      (r (##sys#slot l 1)))
	  (if (eq? (##sys#slot h 1) t)
	      (if prev
		  (set-cdr! prev r)
		  (set! ##sys#timeout-list r))
	      (loop r l))))))

(define (##sys#thread-block-for-timeout! t tm)
  (dbg t " blocks for timeout " tm)
  (unless (and (flonum? tm)			; to catch old code that uses fixum timeouts
	       (fp> tm 0.0))
    (panic "##sys#thread-block-for-timeout!: invalid timeout"))
  ;; This should really use a balanced tree:
  (let loop ([tl ##sys#timeout-list] [prev #f])
    (if (or (null? tl) (fp< tm (caar tl)))
	(if prev
	    (set-cdr! prev (cons (cons tm t) tl))
	    (set! ##sys#timeout-list (cons (cons tm t) tl)) )
	(loop (cdr tl) tl) ) ) 
  (##sys#setslot t 3 'blocked)
  (##sys#setislot t 13 #f)
  (##sys#setslot t 4 tm) )

(define (##sys#thread-block-for-termination! t t2)
  (dbg t " blocks for " t2)
  (let ([state (##sys#slot t2 3)])
    (unless (or (eq? state 'dead) (eq? state 'terminated))
      (##sys#setslot t2 12 (cons t (##sys#slot t2 12)))
      (##sys#setslot t 3 'blocked) 
      (##sys#setislot t 13 #f)
      (##sys#setslot t 11 t2) ) ) )

(define (##sys#abandon-mutexes thread)
  (let ((ms (##sys#slot thread 8)))
    (unless (null? ms)
      (##sys#for-each
       (lambda (m)
	 (##sys#setislot m 2 #f)
	 (##sys#setislot m 4 #t) 
	 (##sys#setislot m 5 #f)
	 (let ((wts (##sys#slot m 3)))
	   (unless (null? wts)
	     (for-each
	      (lambda (t2)
		(dbg "  unblocking: " t2)
		(##sys#thread-unblock! t2) )
	      wts) ) )
	 (##sys#setislot m 3 '()) )
       ms) ) ) )

(define (##sys#thread-kill! t s)
  (dbg "killing: " t " -> " s ", recipients: " (##sys#slot t 12))
  (##sys#abandon-mutexes t)
  (let ((blocked (##sys#slot t 11)))
    (cond
     ((##sys#structure? blocked 'condition-variable)
      (##sys#setslot blocked 2 (##sys#delq t (##sys#slot blocked 2))))
     ((##sys#structure? blocked 'thread)
      (##sys#setslot blocked 12 (##sys#delq t (##sys#slot blocked 12))))) )
  (##sys#remove-from-timeout-list t)
  (##sys#clear-i/o-state-for-thread! t)
  (##sys#setslot t 3 s)
  (##sys#setislot t 4 #f)
  (##sys#setislot t 11 #f)
  (##sys#setislot t 8 '())
  (let ((rs (##sys#slot t 12)))
    (unless (null? rs)
      (for-each
       (lambda (t2)
	 (dbg "  checking: " t2 " (" (##sys#slot t2 3) ") -> " (##sys#slot t2 11))
	 (when (eq? (##sys#slot t2 11) t)
	   (##sys#thread-basic-unblock! t2) ) )
       rs) ) )
  (##sys#setislot t 12 '()) )

(define (##sys#thread-basic-unblock! t)
  (dbg "unblocking: " t)
  (##sys#setislot t 11 #f)		; (FD . RWFLAGS)
  (##sys#setislot t 4 #f)
  (##sys#add-to-ready-queue t) )

(define (##sys#default-exception-handler arg)
  (let ([ct ##sys#current-thread])
    (dbg "exception: " ct " -> " 
	 (if (##sys#structure? arg 'condition) (##sys#slot arg 2) arg))
    (cond ((foreign-value "C_abort_on_thread_exceptions" bool)
	   (let* ([pt ##sys#primordial-thread]
		  [ptx (##sys#slot pt 1)] )
	     (##sys#setslot 
	      pt 1 
	      (lambda ()
		(##sys#signal arg)
		(ptx) ) )
	     (##sys#thread-unblock! pt) ) )
	  (else
	   (##sys#show-exception-warning arg "in thread" ct)))
    (##sys#setslot ct 7 arg)
    (##sys#thread-kill! ct 'terminated)
    (##sys#schedule) ) )


;;; `select()/poll()'-based blocking:

(define ##sys#fd-list '())		; ((FD1 THREAD1 ...) ...)

(define (create-fdset)
  ((foreign-lambda void "C_prepare_fdset" int) (##sys#length ##sys#fd-list))
  (let loop ((lst ##sys#fd-list))
    (unless (null? lst)
      (let ((fd (caar lst)))
	(for-each
	 (lambda (t)
	   (let ((p (##sys#slot t 11)))
	     (fdset-set fd (cdr p))))
	 (cdar lst))
	(loop (cdr lst))))))

(define (fdset-set fd i/o)
  (let ((fdset-add! (foreign-lambda void "C_fdset_add" int bool bool)))
    (dbg "setting fdset for " fd " to " i/o)
    (case i/o
      ((#t #:input) (fdset-add! fd #t #f))
      ((#f #:output) (fdset-add! fd #f #t))
      ((#:all) (fdset-add! fd #t #t))
      (else (panic "fdset-set: invalid i/o direction")))))

(define (fdset-test inf outf i/o)
  (case i/o
    ((#t #:input) inf)
    ((#f #:output) outf)
    ((#:all) (or inf outf))
    (else (panic "fdset-test: invalid i/o direction"))))

(define (##sys#thread-block-for-i/o! t fd i/o)
  (dbg t " blocks for I/O " fd " in mode " i/o)
  #;(unless (memq i/o '(#:all #:input #:output))
    (panic "##sys#thread-block-for-i/o!: invalid i/o mode"))
  (let loop ([lst ##sys#fd-list])
    (if (null? lst) 
	(set! ##sys#fd-list (cons (list fd t) ##sys#fd-list)) 
	(let ([a (car lst)])
	  (if (fx= fd (car a)) 
	      (##sys#setslot a 1 (cons t (cdr a)))
	      (loop (cdr lst)) ) ) ) )
  (##sys#setslot t 3 'blocked)
  (##sys#setislot t 13 #f)
  (##sys#setslot t 11 (cons fd i/o)) )

(define (##sys#unblock-threads-for-i/o)
  (dbg "fd-list: " ##sys#fd-list)
  (create-fdset)
  (let* ((to? (pair? ##sys#timeout-list))
	 (rq? (pair? ready-queue-head))
	 (tmo (if (and to? (not rq?)) ; no thread was unblocked by timeout, so wait
		  (let* ((tmo1 (caar ##sys#timeout-list))
			 (now (##core#inline_allocate ("C_a_i_current_milliseconds" 4) #f)))
		    (fpmax 0.0 (fp- tmo1 now)) )
		  0.0) ) )		; otherwise immediate timeout.
    (dbg "waiting for I/O with timeout " tmo)
    (let ((n ((foreign-lambda int "C_ready_fds_timeout" bool double)
	      (or rq? to?) tmo)))
      (dbg n " fds ready")
      (cond [(eq? -1 n)
	     (dbg "select(2)/poll(2) returned with result -1" )
	     (##sys#force-primordial)]
	    [(fx> n 0)
	     (set! ##sys#fd-list
	       (let loop ((n n) (pos 0) (lst ##sys#fd-list))
		 (if (or (zero? n) (null? lst))
		     lst
		     (let* ((a (car lst))
			    (fd (car a))
			    ;; pos *must* match position of fd in ##sys#fd-list
			    ;; This is checked in C_fd_ready with assert()
			    (inf (##core#inline "C_fd_input_ready" fd pos))
			    (outf (##core#inline "C_fd_output_ready" fd pos)))
		       (dbg "fd " fd " state: input=" inf ", output=" outf)
		       (if (or inf outf)
			   (let loop2 ((threads (cdr a)) (keep '()))
			     (if (null? threads)
				 (if (null? keep)
				     (loop (sub1 n) (add1 pos) (cdr lst))
				     (cons (cons fd keep)
                                           (loop (sub1 n) (add1 pos) (cdr lst))))
				 (let* ((t (car threads))
					(p (##sys#slot t 11)) )
				   (dbg "checking " t " " p)
				   (cond ((##sys#slot t 13) ; unblocked by timeout?
					  (dbg t " unblocked by timeout")
					  (loop2 (cdr threads) keep))
					 ((not (pair? p)) ; not blocked for I/O?
					  ;; thread on fd-list is not blocked for I/O - this
					  ;; is incorrect but will be ignored, just let it run
					  (when (##sys#slot t 4) ; also blocked for timeout?
					    (##sys#remove-from-timeout-list t))
					  (##sys#thread-basic-unblock! t) 
					  (loop2 (cdr threads) keep))
					 ((not (eq? fd (car p)))
					  (panic "thread is registered for I/O on unknown file-descriptor"))
					 ((fdset-test inf outf (cdr p))
					  (when (##sys#slot t 4) ; also blocked for timeout?
					    (##sys#remove-from-timeout-list t))
					  (##sys#thread-basic-unblock! t) 
					  (loop2 (cdr threads) keep))
					 (else (loop2 (cdr threads) (cons t keep)))))))
			   (cons a (loop n (add1 pos) (cdr lst))) ) ) ) ) ) ] ))) )


;;; Clear I/O state for unblocked thread

(define (##sys#clear-i/o-state-for-thread! t)
  (when (pair? (##sys#slot t 11))
    (let ((fd (car (##sys#slot t 11))))
      (set! ##sys#fd-list
	(let loop ((lst ##sys#fd-list))
	  (if (null? lst)
	      '()
	      (let* ((a (car lst))
		     (fd2 (car a)) )
		(if (eq? fd fd2)
		    (let ((ts (##sys#delq t (cdr a)))) ; remove from fd-list entry
		      (cond ((null? ts) (cdr lst))
			    (else
			     (##sys#setslot a 1 ts) ; fd-list entry is list with t removed
			     lst) ) )
		    (cons a (loop (cdr lst)))))))))))


;;; Get list of all threads that are ready or waiting for timeout or waiting for I/O:
;
; (contributed by Joerg Wittenberger)

(define (##sys#all-threads #!optional
			   (cns (lambda (queue arg val init)
				  (cons val init)))
			   (init '()))
  (let loop ((l ready-queue-head) (i init))
    (if (pair? l)
	(loop (cdr l) (cns 'ready #f (car l) i))
	(let loop ((l ##sys#fd-list) (i i))
	  (if (pair? l)
	      (loop (cdr l)
		    (let ((fd (caar l)))
		      (let loop ((l (cdar l)))
			(if (null? l) i
			    (cns 'i/o fd (car l) (loop (cdr l)))))))
	      (let loop ((l ##sys#timeout-list) (i i))
		(if (pair? l)
		    (loop (cdr l) (cns 'timeout (caar l) (cdar l) i))
		    i)))))))


;;; Remove all waiting threads from the relevant queues with the exception of the current thread:

(define (##sys#fetch-and-clear-threads)
  (let ([all (vector ready-queue-head ready-queue-tail ##sys#fd-list ##sys#timeout-list)])
    (set! ready-queue-head '())
    (set! ready-queue-tail '())
    (set! ##sys#fd-list '())
    (set! ##sys#timeout-list '()) 
    all) )


;;; Restore list of waiting threads:

(define (##sys#restore-threads vec)
  (set! ready-queue-head (##sys#slot vec 0))
  (set! ready-queue-tail (##sys#slot vec 1))
  (set! ##sys#fd-list (##sys#slot vec 2))
  (set! ##sys#timeout-list (##sys#slot vec 3)) )


;;; Unblock thread cleanly:

(define (##sys#thread-unblock! t)
  (when (or (eq? 'blocked (##sys#slot t 3))
	    (eq? 'sleeping (##sys#slot t 3)))
    (##sys#remove-from-timeout-list t)
    (##sys#clear-i/o-state-for-thread! t)
    (##sys#thread-basic-unblock! t) ) )
