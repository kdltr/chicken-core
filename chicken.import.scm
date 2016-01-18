;;;; chicken.import.scm - import library for "chicken" module
;
; Copyright (c) 2008-2015, The CHICKEN Team
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


(##sys#register-primitive-module
 'chicken
 '(abort
   add1
   argc+argv
   argv
   bignum?
   blob->string
   blob-size
   blob?
   blob=?
   build-platform
   call/cc
   case-sensitive
   char-name
   (chicken-home . chicken.eval#chicken-home)
   chicken-version
   command-line-arguments
   condition-predicate
   condition-property-accessor
   condition?
   condition->list
   copy-read-table
   cplxnum?
   cpu-time
   current-error-port
   current-exception-handler
   current-milliseconds
   current-read-table
   current-seconds
   (define-reader-ctor . chicken.eval#define-reader-ctor)
   delete-file
   directory-exists?
   (dynamic-load-libraries . chicken.eval#dynamic-load-libraries)
   enable-warnings
   equal=?
   (er-macro-transformer . chicken.expand#er-macro-transformer)
   errno
   error
   (eval-handler . chicken.eval#eval-handler)
   exact-integer?
   exact-integer-sqrt
   exact-integer-nth-root
   exit
   exit-handler
   (expand . chicken.expand#expand)
   (extension-information . chicken.eval#extension-information)
   feature?
   features
   file-exists?
   finite?
   fixnum-bits
   fixnum-precision
   fixnum?
   flonum-decimal-precision
   flonum-epsilon
   flonum-maximum-decimal-exponent
   flonum-maximum-exponent
   flonum-minimum-decimal-exponent
   flonum-minimum-exponent
   flonum-precision
   flonum-print-precision
   flonum-radix
   flonum?
   flush-output
   foldl
   foldr
   force-finalizers
   fp-
   fp*
   fp/
   fp+
   fp<
   fp<=
   fp=
   fp>
   fp>=
   fpabs
   fpacos
   fpasin
   fpatan
   fpatan2
   fpexp
   fpexpt
   fpfloor
   fpceiling
   fpcos
   fpinteger?
   fplog
   fpgcd
   fpmax
   fpmin
   fpneg
   fpround
   fpsin
   fpsqrt
   fptan
   fptruncate
   fx-
   fx*
   fx/
   fx+
   fx<
   fx<=
   fx=
   fx>
   fx>=
   fxand
   fxeven?
   fxgcd
   fxior
   fxlen
   fxmax
   fxmin
   fxmod
   fxneg
   fxnot
   fxodd?
   fxrem
   fxshl
   fxshr
   fxxor
   fxlen
   gensym
   get
   get-call-chain
   get-condition-property
   get-environment-variable
   get-keyword
   (get-line-number . chicken.expand#get-line-number)
   get-output-string
   get-properties
   getter-with-setter
   implicit-exit-handler
   infinite?
   (ir-macro-transformer . chicken.expand#ir-macro-transformer)
   keyword->string
   keyword-style
   keyword?
   (load-library . chicken.eval#load-library)
   (load-noisily . chicken.eval#load-noisily)
   (load-relative . chicken.eval#load-relative)
   (load-verbose . chicken.eval#load-verbose)
   machine-byte-order
   machine-type
   make-blob
   make-composite-condition
   make-parameter
   make-promise
   make-property-condition
   maximum-flonum
   minimum-flonum
   module-environment
   most-negative-fixnum
   most-positive-fixnum
   nan?
   on-exit
   open-input-string
   open-output-string
   parentheses-synonyms
   port-closed?
   port-name
   port-position
   port?
   print
   print-call-chain
   print-error-message
   print*
   procedure-information
   program-name
   promise?
   put!
   quit
   quotient&modulo
   quotient&remainder
   ratnum?
   register-feature!
   remprop!
   rename-file
   (repository-path . chicken.eval#repository-path)
   (require . chicken.eval#require)
   reset
   reset-handler
   return-to-host
   reverse-list->string
   set-parameterized-read-syntax!
   set-port-name!
   set-read-syntax!
   set-sharp-read-syntax!
   setter
   signal
   signum
   singlestep
   software-type
   software-version
   string->blob
   string->keyword
   string->uninterned-symbol
   (strip-syntax . chicken.expand#strip-syntax)
   sub1
   subvector
   symbol-append
   symbol-escape
   symbol-plist
   (syntax-error . chicken.expand#syntax-error)
   system
   unregister-feature!
   vector-resize
   vector-copy!
   void
   warning
   with-exception-handler)
 ##sys#chicken-macro-environment)       ;XXX incorrect - won't work in compiled executable that does expansion
