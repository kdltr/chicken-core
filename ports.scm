;;; ports.scm - Optional non-standard ports
;
; Copyright (c) 2008-2013, The Chicken Team
; Copyright (c) 2000-2007, Felix L. Winkelmann
; All rights reserved.
;
; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions
; are met:
;
;   Redistributions of source code must retain the above copyright
;   notice, this list of conditions and the following disclaimer.
;   Redistributions in binary form must reproduce the above copyright
;   notice, this list of conditions and the following disclaimer in
;   the documentation and/or other materials provided with the
;   distribution.
;   Neither the name of the author nor the names of its contributors
;   may be used to endorse or promote products derived from this
;   software without specific prior written permission.
;
; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
; FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
; COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
; INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
; STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
; OF THE POSSIBILITY OF SUCH DAMAGE.


(declare (unit ports))


(include "common-declarations.scm")

(register-feature! 'ports)


;;;; Port-mapping (found in Gauche):

(define (port-for-each fn thunk)
  (let loop ()
    (let ((x (thunk)))
      (unless (eof-object? x)
	(fn x)
	(loop) ) ) ) )

(define port-map
  (lambda (fn thunk)
    (let loop ((xs '()))
      (let ((x (thunk)))
	(if (eof-object? x)
	    (##sys#fast-reverse xs)
	    (loop (cons (fn x) xs)))))))

(define (port-fold fn acc thunk)
  (let loop ((acc acc))
    (let ((x (thunk)))
      (if (eof-object? x)
          acc
          (loop (fn x acc))) ) ) )

(define-constant +buf-size+ 1024)

(define copy-port 
  (let ((read-char read-char)
	(write-char write-char))
    (define (read-buf port writer)
      (let ((buf (make-string +buf-size+)))
	(let loop ()
	  (let ((n (read-string! +buf-size+ buf port)))
	    (unless (eq? n 0)
	      (writer buf n)
	      (loop))))))
    (define (write-buf buf n port writer)
      (do ((i 0 (fx+ i 1)))
	  ((fx>= i n))
	(writer (integer->char (##sys#byte buf i)) port)))
    (define (read-and-write reader writer)
      (let loop ()
	(let ((x (reader)))
	  (unless (eof-object? x)
	    (writer x)
	    (loop)))))
    (define (read-and-write-buf src dest reader)
      (let ((buf (make-string +buf-size+)))
	(let loop ((n 0))
	  (when (fx>= n +buf-size+)
	    (write-string buf +buf-size+ dest)
	    (set! n 0))
	  (let ((c (reader src)))
	    (cond ((eof-object? c)
		   (when (fx>= n 0)
		     (write-string buf n dest)))
		  (else
		   (##sys#setbyte buf n (char->integer c))
		   (loop (fx+ n 1))))))))
    (lambda (src dest #!optional (read read-char) (write write-char))
      ;; does not check port args intentionally
      (cond ((eq? read read-char)
	     (read-buf
	      src
	      (if (eq? write write-char)
		  (lambda (buf n) (write-string buf n dest))
		  (lambda (buf n) (write-buf buf n dest write)))))
	    ((eq? write write-char)
	     (read-and-write-buf src dest read))
	    (else
	     (read-and-write 
	      (lambda () (read src))
	      (lambda (x) (write x dest))))))))


;;;; funky-ports

(define (make-broadcast-port . ports)
  (make-output-port
   (lambda (s) (for-each (cut write-string s #f <>) ports))
   void
   (lambda () (for-each flush-output ports)) ) )

(define (make-concatenated-port p1 . ports)
  (let ((ports (cons p1 ports)))
    ;;XXX should also forward other port-methods
    (make-input-port
     (lambda ()
       (let loop ()
	 (if (null? ports)
	     #!eof
	     (let ((c (read-char (car ports))))
	       (cond ((eof-object? c)
		      (set! ports (cdr ports))
		      (loop) )
		     (else c) ) ) ) ) )
     (lambda ()
       (and (not (null? ports))
	    (char-ready? (car ports))))
     void
     (lambda ()
       (let loop ()
	 (if (null? ports)
	     #!eof
	     (let ((c (peek-char (car ports))))
	       (cond ((eof-object? c)
		      (set! ports (cdr ports))
		      (loop) )
		     (else c))))))
     (lambda (p n dest start)
       (let loop ((n n) (c 0))
	 (cond ((null? ports) c)
	       ((fx<= n 0) c)
	       (else
		(let ((m (read-string! n dest (car ports) (fx+ start c))))
		  (when (fx< m n)
		    (set! ports (cdr ports)) )
		  (loop (fx- n m) (fx+ c m))))))))))


;;; Redirect standard ports:

(define (with-input-from-port port thunk)
  (##sys#check-input-port port #t 'with-input-from-port)
  (fluid-let ([##sys#standard-input port])
    (thunk) ) )

(define (with-output-to-port port thunk)
  (##sys#check-output-port port #t 'with-output-from-port)
  (fluid-let ([##sys#standard-output port])
    (thunk) ) )

(define (with-error-output-to-port port thunk)
  (##sys#check-output-port port #t 'with-error-output-from-port)
  (fluid-let ([##sys#standard-error port])
    (thunk) ) )

;;; Extended string-port operations:
  
(define call-with-input-string 
  (lambda (str proc)
    (let ((in (open-input-string str)))
      (proc in) ) ) )

(define call-with-output-string
  (lambda (proc)
    (let ((out (open-output-string)))
      (proc out)
      (get-output-string out) ) ) )

(define with-input-from-string
  (lambda (str thunk)
    (fluid-let ([##sys#standard-input (open-input-string str)])
      (thunk) ) ) )

(define with-output-to-string
  (lambda (thunk)
    (fluid-let ([##sys#standard-output (open-output-string)])
      (thunk) 
      (get-output-string ##sys#standard-output) ) ) )


;;; Custom ports:
;
; - Port-slots:
;
;   10: last

(define make-input-port
  (lambda (read ready? close #!optional peek read-string read-line read-buffered)
    (let* ((class
	    (vector 
	     (lambda (p)		; read-char
	       (let ([last (##sys#slot p 10)])
		 (cond [peek (read)]
		       [last
			(##sys#setislot p 10 #f)
			last]
		       [else (read)] ) ) )
	     (lambda (p)		; peek-char
	       (let ([last (##sys#slot p 10)])
		 (cond [peek (peek)]
		       [last last]
		       [else
			(let ([last (read)])
			  (##sys#setslot p 10 last)
			  last) ] ) ) )
	     #f				; write-char
	     #f				; write-string
	     (lambda (p)		; close
	       (close)
	       (##sys#setislot p 8 #t) )
	     #f				; flush-output
	     (lambda (p)		; char-ready?
	       (ready?) )
	     read-string		; read-string!
	     read-line			; read-line
	     read-buffered))
	   (data (vector #f))
	   (port (##sys#make-port #t class "(custom)" 'custom)) )
      (##sys#set-port-data! port data) 
      port) ) )

(define make-output-port
  (lambda (write close #!optional flush)
    (let* ((class
	    (vector
	     #f				; read-char
	     #f				; peek-char
	     (lambda (p c)		; write-char
	       (write (string c)) )
	     (lambda (p s)		; write-string
	       (write s) )
	     (lambda (p)		; close
	       (close)
	       (##sys#setislot p 8 #t) )
	     (lambda (p)		; flush-output
	       (when flush (flush)) )
	     #f				; char-ready?
	     #f				; read-string!
	     #f) )			; read-line
	   (data (vector #f))
	   (port (##sys#make-port #f class "(custom)" 'custom)) )
      (##sys#set-port-data! port data) 
      port) ) )
