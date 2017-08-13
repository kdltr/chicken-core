;;;; chicken.import.scm - import library for "chicken" module
;
; Copyright (c) 2008-2017, The CHICKEN Team
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

;; OBSOLETE: This can be removed after bootstrapping
(if (not (##sys#symbol-has-toplevel-binding? 'chicken.syntax#expand))
    (begin
      (set! chicken.syntax#expand chicken.expand#expand)
      (set! chicken.syntax#er-macro-transformer chicken.expand#er-macro-transformer)
      (set! chicken.syntax#ir-macro-transformer chicken.expand#ir-macro-transformer)
      (set! chicken.syntax#strip-syntax chicken.expand#strip-syntax)
      (set! chicken.syntax#syntax-error chicken.expand#syntax-error)
      (set! chicken.syntax#get-line-number chicken.expand#get-line-number)))

(##sys#register-primitive-module
 'chicken
 '((abort . chicken.condition#abort)
   add1
   argc+argv
   argv
   bignum?
   (build-platform . chicken.platform#build-platform)
   call/cc
   case-sensitive
   char-name
   (chicken-home . chicken.platform#chicken-home)
   (chicken-version . chicken.platform#chicken-version)
   command-line-arguments
   (condition-predicate . chicken.condition#condition-predicate)
   (condition-property-accessor . chicken.condition#condition-property-accessor)
   (condition? . chicken.condition#condition?)
   (condition->list . chicken.condition#condition->list)
   cplxnum?
   current-error-port
   (current-exception-handler . chicken.condition#current-exception-handler)
   delete-file
   directory-exists?
   (dynamic-load-libraries . chicken.load#dynamic-load-libraries)
   enable-warnings
   equal=?
   (er-macro-transformer . chicken.syntax#er-macro-transformer)
   errno
   error
   (eval-handler . chicken.eval#eval-handler)
   exact-integer?
   exact-integer-sqrt
   exact-integer-nth-root
   executable-pathname
   exit
   exit-handler
   (expand . chicken.syntax#expand)
   (feature? . chicken.platform#feature?)
   (features . chicken.platform#features)
   file-exists?
   finite?
   (fixnum-bits . chicken.fixnum#fixnum-bits)
   (fixnum-precision . chicken.fixnum#fixnum-precision)
   fixnum?
   flonum?
   flush-output
   foldl
   foldr
   force-finalizers
   (fx- . chicken.fixnum#fx-)
   (fx* . chicken.fixnum#fx*)
   (fx/ . chicken.fixnum#fx/)
   (fx+ . chicken.fixnum#fx+)
   (fx< . chicken.fixnum#fx<)
   (fx<= . chicken.fixnum#fx<=)
   (fx= . chicken.fixnum#fx=)
   (fx> . chicken.fixnum#fx>)
   (fx>= . chicken.fixnum#fx>=)
   (fxand . chicken.fixnum#fxand)
   (fxeven? . chicken.fixnum#fxeven?)
   (fxgcd . chicken.fixnum#fxgcd)
   (fxior . chicken.fixnum#fxior)
   (fxlen . chicken.fixnum#fxlen)
   (fxmax . chicken.fixnum#fxmax)
   (fxmin . chicken.fixnum#fxmin)
   (fxmod . chicken.fixnum#fxmod)
   (fxneg . chicken.fixnum#fxneg)
   (fxnot . chicken.fixnum#fxnot)
   (fxodd? . chicken.fixnum#fxodd?)
   (fxrem . chicken.fixnum#fxrem)
   (fxshl . chicken.fixnum#fxshl)
   (fxshr . chicken.fixnum#fxshr)
   (fxxor . chicken.fixnum#fxxor)
   (fxlen . chicken.fixnum#fxlen)
   gensym
   get-call-chain
   (get-condition-property . chicken.condition#get-condition-property)
   get-environment-variable
   (get-line-number . chicken.syntax#get-line-number)
   get-output-string
   getter-with-setter
   implicit-exit-handler
   infinite?
   input-port-open?
   (installation-repository . chicken.platform#installation-repository)
   (ir-macro-transformer . chicken.syntax#ir-macro-transformer)
   keyword-style
   (load-library . chicken.load#load-library)
   (load-noisily . chicken.load#load-noisily)
   (load-relative . chicken.load#load-relative)
   (load-verbose . chicken.load#load-verbose)
   (machine-byte-order . chicken.platform#machine-byte-order)
   (machine-type . chicken.platform#machine-type)
   (make-composite-condition . chicken.condition#make-composite-condition)
   make-parameter
   make-promise
   (make-property-condition . chicken.condition#make-property-condition)
   (most-negative-fixnum . chicken.fixnum#most-negative-fixnum)
   (most-positive-fixnum . chicken.fixnum#most-positive-fixnum)
   nan?
   notice
   on-exit
   open-input-string
   open-output-string
   output-port-open?
   parentheses-synonyms
   port-closed?
   port-name
   port-position
   port?
   (provide . chicken.load#provide)
   (provided? . chicken.load#provided?)
   print
   print-call-chain
   print*
   procedure-information
   program-name
   promise?
   quotient&modulo
   quotient&remainder
   ratnum?
   (register-feature! . chicken.platform#register-feature!)
   rename-file
   (repository-path . chicken.platform#repository-path)
   (require . chicken.load#require)
   return-to-host
   reverse-list->string
   set-port-name!
   setter
   (signal . chicken.condition#signal)
   signum
   singlestep
   sleep
   (software-type . chicken.platform#software-type)
   (software-version . chicken.platform#software-version)
   string->uninterned-symbol
   (strip-syntax . chicken.syntax#strip-syntax)
   sub1
   subvector
   symbol-append
   symbol-escape
   (syntax-error . chicken.syntax#syntax-error)
   system
   (unregister-feature! . chicken.platform#unregister-feature!)
   vector-resize
   vector-copy!
   void
   warning
   (with-exception-handler . chicken.condition#with-exception-handler))
 ##sys#chicken-macro-environment)       ;XXX incorrect - won't work in compiled executable that does expansion
