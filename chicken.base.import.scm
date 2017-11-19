;;;; chicken.base.import.scm - import library for "chicken.base" module
;
; Copyright (c) 2017, The CHICKEN Team
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

(##sys#register-core-module
 'chicken.base
 'library
 '((add1 . chicken.base#add1)
   (bignum? . chicken.base#bignum?)
   (call/cc . chicken.base#call/cc)
   (char-name . chicken.base#char-name)
   (cplxnum? . chicken.base#cplxnum?)
   (current-error-port . chicken.base#current-error-port)
   (emergency-exit . chicken.base#emergency-exit)
   (enable-warnings . chicken.base#enable-warnings)
   (equal=? . chicken.base#equal=?)
   (exit . chicken.base#exit)
   (error . chicken.base#error)
   (exact-integer? . chicken.base#exact-integer?)
   (exact-integer-sqrt . chicken.base#exact-integer-sqrt)
   (exact-integer-nth-root . chicken.base#exact-integer-nth-root)
   (exit-handler . chicken.base#exit-handler)
   (finite? . chicken.base#finite?)
   (fixnum? . chicken.base#fixnum?)
   (flonum? . chicken.base#flonum?)
   (foldl . chicken.base#foldl)
   (foldr . chicken.base#foldr)
   (gensym . chicken.base#gensym)
   (get-call-chain . chicken.base#get-call-chain)
   (getter-with-setter . chicken.base#getter-with-setter)
   (implicit-exit-handler . chicken.base#implicit-exit-handler)
   (infinite? . chicken.base#infinite?)
   (make-parameter . chicken.base#make-parameter)
   (make-promise . chicken.base#make-promise)
   (nan? . chicken.base#nan?)
   (notice . chicken.base#notice)
   (on-exit . chicken.base#on-exit)
   (print . chicken.base#print)
   (print-call-chain . chicken.base#print-call-chain)
   (print* . chicken.base#print*)
   (procedure-information . chicken.base#procedure-information)
   (promise? . chicken.base#promise?)
   (quotient&modulo . chicken.base#quotient&modulo)
   (quotient&remainder . chicken.base#quotient&remainder)
   (ratnum? . chicken.base#ratnum?)
   (setter . chicken.base#setter)
   (signum . chicken.base#signum)
   (string->uninterned-symbol . chicken.base#string->uninterned-symbol)
   (sub1 . chicken.base#sub1)
   (subvector . chicken.base#subvector)
   (symbol-append . chicken.base#symbol-append)
   (vector-copy! . chicken.base#vector-copy!)
   (vector-resize . chicken.base#vector-resize)
   (void . chicken.base#void)
   (warning . chicken.base#warning))
 ;; OBSOLETE: This can be removed after bootstrapping
 (if (##sys#symbol-has-toplevel-binding? '##sys#chicken.base-macro-environment)
     ##sys#chicken.base-macro-environment
     ##sys#chicken-macro-environment))
