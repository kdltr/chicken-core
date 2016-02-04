;;;; internal.scm - Runtime support module for CHICKEN
;
; Copyright (c) 2008-2015, The CHICKEN Team
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
  (unit internal)
  (disable-interrupts)
  (fixnum))

(module chicken.internal
  (library-id valid-library-specifier?
   module-requirement string->c-identifier)

(import scheme chicken)

(include "common-declarations.scm")
(include "mini-srfi-1.scm")


;;; Convert string into valid C-identifier:

(define (string->c-identifier str)
  (let ((out (open-output-string))
	(n (string-length str)))
    (do ((i 0 (fx+ i 1)))
	((fx>= i n) (get-output-string out))
      (let ((c (string-ref str i)))
	(if (and (not (char-alphabetic? c))
		 (or (not (char-numeric? c)) (fx= i 0)))
	    (let ((i (char->integer c)))
	      (write-char #\_ out)
	      (when (fx< i 16) (write-char #\0 out))
	      (display (number->string i 16) out))
	    (write-char c out))))))


;;; Parse library specifications:

(define (valid-library-specifier? x)
  (or (symbol? x)
      (and (list? x)
	   (not (null? x))
	   (every (lambda (x) (or (symbol? x) (fixnum? x))) x))))

(define (library-id lib)
  (define (fail)
    (##sys#error "invalid library specifier" lib))
  (define (srfi? x)
    (and (pair? (cdr x))
	 (null? (cddr x))
	 (eq? 'srfi (car x))
	 (fixnum? (cadr x))))
  (define (library-part->string x)
    (cond ((symbol? x) (##sys#symbol->string x))
	  ((fixnum? x) (##sys#number->string x))
	  (else (fail))))
  (cond
    ((symbol? lib) lib)
    ((null? lib) (fail))
    ((not (list? lib)) (fail))
    ((srfi? lib)
     (##sys#intern-symbol
      (##sys#string-append "srfi-" (##sys#number->string (cadr lib)))))
    (else
     (do ((lst (cdr lib) (cdr lst))
	  (str (library-part->string (car lib))
	       (string-append str "." (library-part->string (car lst)))))
	 ((null? lst)
	  (##sys#intern-symbol str))))))


;;; Requirement identifier for modules:

(define (module-requirement id)
  (##sys#string->symbol
   (##sys#string-append (##sys#slot id 1) "#")))


) ; chicken.internal
