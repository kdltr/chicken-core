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
  (srfi-id library-id)

(import scheme chicken)

(define (srfi-id n)
  (if (fixnum? n)
      (##sys#intern-symbol
       (##sys#string-append "srfi-" (##sys#number->string n)))
      (##sys#error "invalid SRFI number" n)))

(define (library-id lib)
  (define (library-part->string id)
    (cond ((symbol? id) (##sys#symbol->string id))
	  ((number? id) (##sys#number->string id))
	  (else (##sys#error "invalid library specifier" lib))))
  (cond
    ((symbol? lib) lib)
    ((list? lib)
     (do ((lib (cdr lib) (cdr lib))
	  (str (library-part->string (car lib))
	       (string-append str "." (library-part->string (car lib)))))
	 ((null? lib) (##sys#intern-symbol str))))
    (else (##sys#error "invalid library specifier" lib))))

) ; chicken.internal
