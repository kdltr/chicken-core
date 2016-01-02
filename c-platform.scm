;;;; c-platform.scm - Platform specific parameters and definitions
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


;; TODO: Rename c-platform back to "platform" and turn it into a
;; functor?  This may require the creation of an additional file.
;; Same goes for "backend" and "driver".
(declare
  (unit c-platform)
  (uses data-structures
	optimizer support compiler))

(module chicken.compiler.c-platform
    (default-declarations default-profiling-declarations
     units-used-by-default
     valid-compiler-options valid-compiler-options-with-argument

     ;; For consumption by c-backend *only*
     target-include-file words-per-flonum)

(import chicken scheme
	chicken.data-structures
	chicken.compiler.optimizer
	chicken.compiler.support
	chicken.compiler.core)

(include "tweaks")
(include "mini-srfi-1.scm")

;;; Parameters:

(default-optimization-passes 3)

(define default-declarations
  '((always-bound
     ##sys#standard-input ##sys#standard-output ##sys#standard-error
     ##sys#undefined-value)
    (bound-to-procedure
     ##sys#for-each ##sys#map ##sys#print ##sys#setter
     ##sys#setslot ##sys#dynamic-wind ##sys#call-with-values
     ##sys#start-timer ##sys#stop-timer ##sys#gcd ##sys#lcm ##sys#make-promise ##sys#structure? ##sys#slot 
     ##sys#allocate-vector ##sys#list->vector ##sys#block-ref ##sys#block-set!
     ##sys#list ##sys#cons ##sys#append ##sys#vector ##sys#foreign-char-argument ##sys#foreign-fixnum-argument
     ##sys#foreign-flonum-argument ##sys#error ##sys#peek-c-string ##sys#peek-nonnull-c-string 
     ##sys#peek-and-free-c-string ##sys#peek-and-free-nonnull-c-string
     ##sys#foreign-block-argument ##sys#foreign-string-argument ##sys#foreign-pointer-argument ##sys#foreign-integer-argument
     ##sys#call-with-current-continuation) ) )

(define default-profiling-declarations
  '((##core#declare
     (uses profiler)
     (bound-to-procedure
       ##sys#profile-entry ##sys#profile-exit) ) ) )

(define units-used-by-default '(library eval chicken-syntax))
(define words-per-flonum 4)

(eq-inline-operator "C_eqp")
(membership-test-operators
  '(("C_i_memq" . "C_eqp") ("C_u_i_memq" . "C_eqp") ("C_i_member" . "C_i_equalp")
    ("C_i_memv" . "C_i_eqvp") ) )
(membership-unfold-limit 20)
(define target-include-file "chicken.h")

(define valid-compiler-options
  '(-help 
    h help version verbose explicit-use 
    no-trace no-warnings unsafe block 
    check-syntax to-stdout no-usual-integrations case-insensitive no-lambda-info 
    profile inline keep-shadowed-macros ignore-repository
    fixnum-arithmetic disable-interrupts optimize-leaf-routines
    compile-syntax tag-pointers accumulate-profile
    disable-stack-overflow-checks raw specialize
    emit-external-prototypes-first release local inline-global
    analyze-only dynamic
    no-argc-checks no-procedure-checks no-parentheses-synonyms
    no-procedure-checks-for-toplevel-bindings
    no-bound-checks no-procedure-checks-for-usual-bindings no-compiler-syntax
    no-parentheses-synonyms no-symbol-escape r5rs-syntax emit-all-import-libraries
    strict-types clustering lfa2 debug-info
    setup-mode no-module-registration) )

(define valid-compiler-options-with-argument
  '(debug 
    output-file include-path heap-size stack-size unit uses module
    keyword-style require-extension inline-limit profile-name
    prelude postlude prologue epilogue nursery extend feature no-feature
    emit-inline-file consult-inline-file
    emit-type-file consult-type-file
    emit-import-library))


;;; Standard and extended bindings:

(set! default-standard-bindings
  '(not boolean? apply call-with-current-continuation eq? eqv? equal? pair? cons car cdr caar cadr
    cdar cddr caaar caadr cadar caddr cdaar cdadr cddar cdddr caaaar caaadr caadar caaddr cadaar
    cadadr caddar cadddr cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr set-car! set-cdr!
    null? list list? length zero? * - + / - > < >= <= = current-output-port current-input-port
    write-char newline write display append symbol->string for-each map char? char->integer
    integer->char eof-object? vector-length string-length string-ref string-set! vector-ref 
    vector-set! char=? char<? char>? char>=? char<=? gcd lcm reverse symbol? string->symbol
    number? complex? real? integer? rational? odd? even? positive? negative? exact? inexact?
    max min quotient remainder modulo floor ceiling truncate round rationalize
    exact->inexact inexact->exact
    exp log sin expt sqrt cos tan asin acos atan number->string string->number char-ci=?
    char-ci<? char-ci>? char-ci>=? char-ci<=? char-alphabetic? char-whitespace? char-numeric?
    char-lower-case? char-upper-case? char-upcase char-downcase string? string=? string>? string<?
    string>=? string<=? string-ci=? string-ci<? string-ci>? string-ci<=? string-ci>=?
    string-append string->list list->string vector? vector->list list->vector string read
    read-char substring string-fill! vector-fill! make-string make-vector open-input-file
    open-output-file call-with-input-file call-with-output-file close-input-port close-output-port
    values call-with-values vector procedure? memq memv member assq assv assoc list-tail
    list-ref abs char-ready? peek-char list->string string->list
    current-input-port current-output-port) )

(set! default-extended-bindings
  '(bignum? cplxnum? ratnum? bitwise-and bitwise-ior bitwise-xor bitwise-not
    add1 sub1 fx+ fx- fx* fx/ fxgcd fx+? fx-? fx*? fx/? fxmod fxrem fp/?
    fx= fx> fx< fx>= fx<= fixnum? fxneg fxmax fxmin fxlen fxand fxnot fxior
    fxxor fxshr fxshl bit-set? fxodd? fxeven? fp+ fp- fp* fp/ fpmin fpmax fpneg
    fpgcd fp> fp< fp= fp>= fp<= fpfloor fpceiling fptruncate fpround fpsin
    fpcos fptan fpasin fpacos fpatan fpatan2 fpexp fpexpt fplog fpsqrt fpabs
    fpinteger? exact-integer? flonum? nan? finite? infinite? integer-length
    arithmetic-shift void flush-output print print* error call/cc blob-size
    identity blob=? equal=? make-polar make-rectangular real-part imag-part
    string->symbol symbol-append foldl foldr setter
    current-error-port current-thread get-keyword
    srfi-4#u8vector-length srfi-4#s8vector-length
    srfi-4#u16vector-length srfi-4#s16vector-length
    srfi-4#u32vector-length srfi-4#u64vector-length
    srfi-4#s32vector-length srfi-4#s64vector-length
    srfi-4#f32vector-length srfi-4#f64vector-length
    srfi-4#u8vector->blob/shared srfi-4#s8vector->blob/shared
    srfi-4#u16vector->blob/shared srfi-4#s16vector->blob/shared
    srfi-4#u32vector->blob/shared srfi-4#s32vector->blob/shared
    srfi-4#u64vector->blob/shared srfi-4#s64vector->blob/shared
    srfi-4#f32vector->blob/shared srfi-4#f64vector->blob/shared
    srfi-4#blob->u8vector/shared srfi-4#blob->s8vector/shared
    srfi-4#blob->u16vector/shared srfi-4#blob->s16vector/shared
    srfi-4#blob->u32vector/shared srfi-4#blob->s32vector/shared
    srfi-4#blob->u64vector/shared srfi-4#blob->s64vector/shared
    srfi-4#blob->f32vector/shared srfi-4#blob->f64vector/shared
    chicken.lolevel#make-record-instance
    chicken.lolevel#block-ref chicken.lolevel#block-set!
    chicken.lolevel#u8vector-ref chicken.lolevel#s8vector-ref
    chicken.lolevel#u16vector-ref chicken.lolevel#s16vector-ref
    chicken.lolevel#u32vector-ref chicken.lolevel#s32vector-ref
    chicken.lolevel#u64vector-ref chicken.lolevel#s64vector-ref
    chicken.lolevel#f32vector-ref chicken.lolevel#f64vector-ref
    chicken.lolevel#f32vector-set! chicken.lolevel#f64vector-set!
    chicken.lolevel#u8vector-set! chicken.lolevel#s8vector-set!
    chicken.lolevel#u16vector-set! chicken.lolevel#s16vector-set!
    chicken.lolevel#u32vector-set! chicken.lolevel#s32vector-set!
    chicken.lolevel#u64vector-set! chicken.lolevel#s64vector-set!
    chicken.lolevel#locative-ref chicken.lolevel#locative-set!
    chicken.lolevel#locative->object chicken.lolevel#locative?
    chicken.lolevel#pointer->object chicken.lolevel#pointer+
    chicken.lolevel#address->pointer chicken.lolevel#pointer->address
    chicken.lolevel#pointer=? chicken.lolevel#number-of-slots
    chicken.lolevel#pointer-u8-ref chicken.lolevel#pointer-s8-ref
    chicken.lolevel#pointer-u16-ref chicken.lolevel#pointer-s16-ref
    chicken.lolevel#pointer-u32-ref chicken.lolevel#pointer-s32-ref
    chicken.lolevel#pointer-f32-ref chicken.lolevel#pointer-f64-ref
    chicken.lolevel#pointer-u8-set! chicken.lolevel#pointer-s8-set!
    chicken.lolevel#pointer-u16-set! chicken.lolevel#pointer-s16-set!
    chicken.lolevel#pointer-u32-set! chicken.lolevel#pointer-s32-set!
    chicken.lolevel#pointer-f32-set! chicken.lolevel#pointer-f64-set!
    chicken.data-structures#o
    chicken.data-structures#substring-index
    chicken.data-structures#substring-index-ci
    chicken.data-structures#substring=?
    chicken.data-structures#substring-ci=?
    chicken.data-structures#any? chicken.data-structures#atom?
    chicken.data-structures#alist-ref chicken.data-structures#rassoc
    chicken.extras#read-string chicken.extras#format
    chicken.extras#printf chicken.extras#sprintf chicken.extras#fprintf))

(set! internal-bindings
  '(##sys#slot ##sys#setslot ##sys#block-ref ##sys#block-set!
    ##sys#call-with-current-continuation ##sys#size ##sys#byte ##sys#setbyte
    ##sys#pointer? ##sys#generic-structure? ##sys#structure? ##sys#check-structure
    ##sys#check-exact ##sys#check-number ##sys#check-list ##sys#check-pair ##sys#check-string
    ##sys#check-symbol ##sys#check-boolean ##sys#check-locative
    ##sys#check-port ##sys#check-input-port ##sys#check-output-port
    ##sys#check-open-port
    ##sys#check-char ##sys#check-vector ##sys#check-byte-vector ##sys#list ##sys#cons
    ##sys#call-with-values ##sys#flonum-in-fixnum-range? 
    ##sys#fudge ##sys#immediate? ##sys#context-switch
    ##sys#make-structure ##sys#apply ##sys#apply-values ##sys#continuation-graft
    ##sys#bytevector? ##sys#make-vector ##sys#setter ##sys#car ##sys#cdr ##sys#pair?
    ##sys#eq? ##sys#list? ##sys#vector? ##sys#eqv? ##sys#get-keyword
    ##sys#foreign-char-argument ##sys#foreign-fixnum-argument ##sys#foreign-flonum-argument
    ##sys#foreign-block-argument ##sys#foreign-struct-wrapper-argument
    ##sys#foreign-string-argument ##sys#foreign-pointer-argument ##sys#void
    ##sys#foreign-ranged-integer-argument ##sys#foreign-unsigned-ranged-integer-argument
    ##sys#peek-fixnum ##sys#setislot ##sys#poke-integer ##sys#permanent? ##sys#values ##sys#poke-double
    ##sys#intern-symbol ##sys#make-symbol ##sys#null-pointer? ##sys#peek-byte
    ##sys#file-exists? ##sys#substring-index ##sys#substring-index-ci ##sys#lcm ##sys#gcd))

(for-each
 (cut mark-variable <> '##compiler#pure '#t)
 '(##sys#slot ##sys#block-ref ##sys#size ##sys#byte
    ##sys#pointer? ##sys#generic-structure? ##sys#fudge ##sys#immediate?
    ##sys#bytevector? ##sys#pair? ##sys#eq? ##sys#list? ##sys#vector? ##sys#eqv? 
    ##sys#get-keyword			; ok it isn't, but this is only used for ext. llists
    ##sys#void ##sys#permanent?))


;;; Rewriting-definitions for this platform:

(rewrite '+ 19 0 "C_fixnum_plus" "C_u_fixnum_plus" #f)

(let ()
  ;; (add1 <x>) -> (##core#inline "C_fixnum_increase" <x>)     [fixnum-mode]
  ;; (add1 <x>) -> (##core#inline "C_u_fixnum_increase" <x>)   [fixnum-mode + unsafe]
  ;; (add1 <x>) -> (##core#inline_allocate ("C_s_a_i_plus" 36) <x> 1) 
  ;; (sub1 <x>) -> (##core#inline "C_fixnum_decrease" <x>)     [fixnum-mode]
  ;; (sub1 <x>) -> (##core#inline "C_u_fixnum_decrease" <x>)   [fixnum-mode + unsafe]
  ;; (sub1 <x>) -> (##core#inline_allocate ("C_s_a_i_minus" 36) <x> 1) 
  (define ((op1 fiop ufiop aiop) db classargs cont callargs)
    (and (= (length callargs) 1)
	 (make-node
	  '##core#call (list #t)
	  (list 
	   cont
	   (if (eq? 'fixnum number-type)
	       (make-node '##core#inline (list (if unsafe ufiop fiop)) callargs)
	       (make-node
		'##core#inline_allocate (list aiop 36)
		(list (car callargs) (qnode 1))))))))
  (rewrite 'add1 8 (op1 "C_fixnum_increase" "C_u_fixnum_increase" "C_s_a_i_plus"))
  (rewrite 'sub1 8 (op1 "C_fixnum_decrease" "C_u_fixnum_decrease" "C_s_a_i_minus")))

(let ()
  (define (eqv?-id db classargs cont callargs)
    ;; (eqv? <var> <var>) -> (quote #t)          [two identical objects]
    ;; (eqv? ...) -> (##core#inline "C_eqp" ...)
    ;; [one argument is a constant and either immediate or not a number]
    (and (= (length callargs) 2)
	 (let ((arg1 (first callargs))
	       (arg2 (second callargs)) )
	   (or (and (eq? '##core#variable (node-class arg1))
		    (eq? '##core#variable (node-class arg2))
		    (equal? (node-parameters arg1) (node-parameters arg2))
		    (make-node '##core#call (list #t) (list cont (qnode #t))) )
	       (and (or (and (eq? 'quote (node-class arg1))
			     (let ((p1 (first (node-parameters arg1))))
			       (or (immediate? p1) (not (number? p1)))) )
			(and (eq? 'quote (node-class arg2))
			     (let ((p2 (first (node-parameters arg2))))
			       (or (immediate? p2) (not (number? p2)))) ) )
		    (make-node
		     '##core#call (list #t) 
		     (list cont (make-node '##core#inline '("C_eqp") callargs)) ) ) ) ) ) )
  (rewrite 'eqv? 8 eqv?-id)
  (rewrite '##sys#eqv? 8 eqv?-id))

(rewrite
 'equal? 8
 (lambda (db classargs cont callargs)
   ;; (equal? <var> <var>) -> (quote #t)
   ;; (equal? ...) -> (##core#inline "C_eqp" ...) [one argument is a constant and immediate or a symbol]
   ;; (equal? ...) -> (##core#inline "C_i_equalp" ...)
   (and (= (length callargs) 2)
	(let ([arg1 (first callargs)]
	      [arg2 (second callargs)] )
	  (or (and (eq? '##core#variable (node-class arg1))
		   (eq? '##core#variable (node-class arg2))
		   (equal? (node-parameters arg1) (node-parameters arg2))
		   (make-node '##core#call (list #t) (list cont (qnode #t))) )
	      (and (or (and (eq? 'quote (node-class arg1))
			    (let ([f (first (node-parameters arg1))])
			      (or (immediate? f) (symbol? f)) ) )
		       (and (eq? 'quote (node-class arg2))
			    (let ([f (first (node-parameters arg2))])
			      (or (immediate? f) (symbol? f)) ) ) )
		   (make-node
		    '##core#call (list #t) 
		    (list cont (make-node '##core#inline '("C_eqp") callargs)) ) )
	      (make-node
	       '##core#call (list #t) 
	       (list cont (make-node '##core#inline '("C_i_equalp") callargs)) ) ) ) ) ) )

(let ()
  (define (rewrite-apply db classargs cont callargs)
    ;; (apply <fn> <x1> ... '(<y1> ...)) -> (<fn> <x1> ... '<y1> ...)
    ;; (apply ...) -> ((##core#proc "C_apply") ...)
    ;; (apply values <lst>) -> ((##core#proc "C_apply_values") lst)
    ;; (apply ##sys#values <lst>) -> ((##core#proc "C_apply_values") lst)
    (and (pair? callargs)
	 (let ([lastarg (last callargs)]
	       [proc (car callargs)] )
	   (if (eq? 'quote (node-class lastarg))
	       (make-node
		'##core#call (list #f)
		(cons* (first callargs)
		       cont 
		       (append (cdr (butlast callargs)) (map qnode (first (node-parameters lastarg)))) ) )
	       (or (and (eq? '##core#variable (node-class proc))
			(= 2 (length callargs))
			(let ([name (car (node-parameters proc))])
			  (and (memq name '(values ##sys#values))
			       (intrinsic? name)
			       (make-node
				'##core#call (list #t)
				(list (make-node '##core#proc '("C_apply_values" #t) '())
				      cont
				      (cadr callargs) ) ) ) ) ) 
		   (make-node
		    '##core#call (list #t)
		    (cons* (make-node '##core#proc '("C_apply" #t) '())
			   cont callargs) ) ) ) ) ) )
  (rewrite 'apply 8 rewrite-apply)
  (rewrite '##sys#apply 8 rewrite-apply) )

(let ()
  (define (rewrite-c..r op iop1 iop2)
    (rewrite
     op 8
     (lambda (db classargs cont callargs)
       ;; (<op> <x>) -> (##core#inline <iop1> <x>) [safe]
       ;; (<op> <x>) -> (##core#inline <iop2> <x>) [unsafe]
       (and (= (length callargs) 1)
	    (call-with-current-continuation
	     (lambda (return)
	       (let ((arg (first callargs)))
		 (make-node
		  '##core#call (list #t)
		  (list
		   cont
		   (cond [(and unsafe iop2) (make-node '##core#inline (list iop2) callargs)]
			 [iop1 (make-node '##core#inline (list iop1) callargs)]
			 [else (return #f)] ) ) ) ) ) ) ) ) ) )

  (rewrite-c..r 'car "C_i_car" "C_u_i_car")
  (rewrite-c..r '##sys#car "C_i_car" "C_u_i_car")
  (rewrite-c..r '##sys#cdr "C_i_cdr" "C_u_i_cdr")
  (rewrite-c..r 'cadr "C_i_cadr" "C_u_i_cadr")
  (rewrite-c..r 'caddr "C_i_caddr" "C_u_i_caddr")
  (rewrite-c..r 'cadddr "C_i_cadddr" "C_u_i_cadddr") )

(let ([rvalues
       (lambda (db classargs cont callargs)
	 ;; (values <x>) -> <x>
	 (and (= (length callargs) 1)
	      (make-node '##core#call (list #t) (cons cont callargs) ) ) ) ] )
  (rewrite 'values 8 rvalues)
  (rewrite '##sys#values 8 rvalues) )

(let ()
  (define (rewrite-c-w-v db classargs cont callargs)
   ;; (call-with-values <var1> <var2>) -> (let ((k (lambda (r) [<var2> <k0> r]))) [<var1> k])
   ;; - if <var2> is a known lambda of a single argument
   (and (= 2 (length callargs))
	(let ((arg1 (car callargs))
	      (arg2 (cadr callargs)) )
	  (and (eq? '##core#variable (node-class arg1))	; probably not needed
	       (eq? '##core#variable (node-class arg2))
	       (and-let* ((sym (car (node-parameters arg2)))
			  (val (db-get db sym 'value)) )
		 (and (eq? '##core#lambda (node-class val))
		      (let ((llist (third (node-parameters val))))
			(and (list? llist)
			     (= 2 (length llist))
			     (let ((tmp (gensym))
				   (tmpk (gensym 'r)) )
			       (debugging 'o "removing single-valued `call-with-values'" (node-parameters val))
			       (make-node
				'let (list tmp)
				(list (make-node
				       '##core#lambda
				       (list (gensym 'f_) #f (list tmpk) 0)
				       (list (make-node
					      '##core#call (list #t)
					      (list arg2 cont (varnode tmpk)) ) ) ) 
				      (make-node
				       '##core#call (list #t)
				       (list arg1 (varnode tmp)) ) ) ) ) ) ) ) ) ) ) ) )
  (rewrite 'call-with-values 8 rewrite-c-w-v)
  (rewrite '##sys#call-with-values 8 rewrite-c-w-v) )

(rewrite 'values 13 #f "C_values" #t)
(rewrite '##sys#values 13 #f "C_values" #t)
(rewrite 'call-with-values 13 2 "C_u_call_with_values" #f)
(rewrite 'call-with-values 13 2 "C_call_with_values" #t)
(rewrite '##sys#call-with-values 13 2 "C_u_call_with_values" #f)
(rewrite '##sys#call-with-values 13 2 "C_call_with_values" #t)
(rewrite 'chicken.lolevel#locative-ref 13 1 "C_locative_ref" #t)
(rewrite '##sys#continuation-graft 13 2 "C_continuation_graft" #t)

(rewrite 'caar 2 1 "C_u_i_caar" #f)
(rewrite 'cdar 2 1 "C_u_i_cdar" #f)
(rewrite 'cddr 2 1 "C_u_i_cddr" #f)
(rewrite 'caaar 2 1 "C_u_i_caaar" #f)
(rewrite 'cadar 2 1 "C_u_i_cadar" #f)
(rewrite 'caddr 2 1 "C_u_i_caddr" #f)
(rewrite 'cdaar 2 1 "C_u_i_cdaar" #f)
(rewrite 'cdadr 2 1 "C_u_i_cdadr" #f)
(rewrite 'cddar 2 1 "C_u_i_cddar" #f)
(rewrite 'cdddr 2 1 "C_u_i_cdddr" #f)
(rewrite 'caaaar 2 1 "C_u_i_caaaar" #f)
(rewrite 'caadar 2 1 "C_u_i_caadar" #f)
(rewrite 'caaddr 2 1 "C_u_i_caaddr" #f)
(rewrite 'cadaar 2 1 "C_u_i_cadaar" #f)
(rewrite 'cadadr 2 1 "C_u_i_cadadr" #f)
(rewrite 'caddar 2 1 "C_u_i_caddar" #f)
(rewrite 'cadddr 2 1 "C_u_i_cadddr" #f)
(rewrite 'cdaaar 2 1 "C_u_i_cdaaar" #f)
(rewrite 'cdaadr 2 1 "C_u_i_cdaadr" #f)
(rewrite 'cdadar 2 1 "C_u_i_cdadar" #f)
(rewrite 'cdaddr 2 1 "C_u_i_cdaddr" #f)
(rewrite 'cddaar 2 1 "C_u_i_cddaar" #f)
(rewrite 'cddadr 2 1 "C_u_i_cddadr" #f)
(rewrite 'cdddar 2 1 "C_u_i_cdddar" #f)
(rewrite 'cddddr 2 1 "C_u_i_cddddr" #f)

(rewrite 'caar 2 1 "C_i_caar" #t)
(rewrite 'cdar 2 1 "C_i_cdar" #t)
(rewrite 'cddr 2 1 "C_i_cddr" #t)
(rewrite 'cdddr 2 1 "C_i_cdddr" #t)
(rewrite 'cddddr 2 1 "C_i_cddddr" #t)

(rewrite 'cdr 7 1 "C_slot" 1 #f)
(rewrite 'cdr 2 1 "C_i_cdr" #t)

(rewrite 'eq? 1 2 "C_eqp")
(rewrite '##sys#eq? 1 2 "C_eqp")
(rewrite 'eqv? 1 2 "C_i_eqvp")
(rewrite '##sys#eqv? 1 2 "C_i_eqvp")

(rewrite 'list-ref 2 2 "C_u_i_list_ref" #f)
(rewrite 'list-ref 2 2 "C_i_list_ref" #t)
(rewrite 'null? 2 1 "C_i_nullp" #t)
(rewrite '##sys#null? 2 1 "C_i_nullp" #t)
(rewrite 'length 2 1 "C_i_length" #t)
(rewrite 'not 2 1 "C_i_not"#t )
(rewrite 'char? 2 1 "C_charp" #t)
(rewrite 'string? 2 1 "C_i_stringp" #t)
(rewrite 'chicken.lolevel#locative? 2 1 "C_i_locativep" #t)
(rewrite 'symbol? 2 1 "C_i_symbolp" #t)
(rewrite 'vector? 2 1 "C_i_vectorp" #t)
(rewrite '##sys#vector? 2 1 "C_i_vectorp" #t)
(rewrite 'pair? 2 1 "C_i_pairp" #t)
(rewrite '##sys#pair? 2 1 "C_i_pairp" #t)
(rewrite 'procedure? 2 1 "C_i_closurep" #t)
(rewrite 'port? 2 1 "C_i_portp" #t)
(rewrite 'boolean? 2 1 "C_booleanp" #t)
(rewrite 'number? 2 1 "C_i_numberp" #t)
(rewrite 'complex? 2 1 "C_i_numberp" #t)
(rewrite 'rational? 2 1 "C_i_rationalp" #t)
(rewrite 'real? 2 1 "C_i_realp" #t)
(rewrite 'integer? 2 1 "C_i_integerp" #t)
(rewrite 'exact-integer? 2 1 "C_i_exact_integerp" #t)
(rewrite 'flonum? 2 1 "C_i_flonump" #t)
(rewrite 'fixnum? 2 1 "C_fixnump" #t)
(rewrite 'bignum? 2 1 "C_i_bignump" #t)
(rewrite 'cplxnum? 2 1 "C_i_cplxnump" #t)
(rewrite 'ratnum? 2 1 "C_i_ratnump" #t)
(rewrite 'nan? 2 1 "C_i_nanp" #f)
(rewrite 'finite? 2 1 "C_i_finitep" #f)
(rewrite 'infinite? 2 1 "C_i_infinitep" #f)
(rewrite 'fpinteger? 2 1 "C_u_i_fpintegerp" #f)
(rewrite '##sys#pointer? 2 1 "C_anypointerp" #t)
(rewrite 'pointer? 2 1 "C_i_safe_pointerp" #t)
(rewrite '##sys#generic-structure? 2 1 "C_structurep" #t)
(rewrite 'exact? 2 1 "C_i_exactp" #t)
(rewrite 'exact? 2 1 "C_u_i_exactp" #f)
(rewrite 'inexact? 2 1 "C_i_inexactp" #t)
(rewrite 'inexact? 2 1 "C_u_i_inexactp" #f)
(rewrite 'list? 2 1 "C_i_listp" #t)
(rewrite 'proper-list? 2 1 "C_i_listp" #t)
(rewrite 'eof-object? 2 1 "C_eofp" #t)
(rewrite 'string-ref 2 2 "C_subchar" #f)
(rewrite 'string-ref 2 2 "C_i_string_ref" #t)
(rewrite 'string-set! 2 3 "C_setsubchar" #f)
(rewrite 'string-set! 2 3 "C_i_string_set" #t)
(rewrite 'vector-ref 2 2 "C_slot" #f)
(rewrite 'vector-ref 2 2 "C_i_vector_ref" #t)
(rewrite 'char=? 2 2 "C_u_i_char_equalp" #f)
(rewrite 'char=? 2 2 "C_i_char_equalp" #t)
(rewrite 'char>? 2 2 "C_u_i_char_greaterp" #f)
(rewrite 'char>? 2 2 "C_i_char_greaterp" #t)
(rewrite 'char<? 2 2 "C_u_i_char_lessp" #f)
(rewrite 'char<? 2 2 "C_i_char_lessp" #t)
(rewrite 'char>=? 2 2 "C_u_i_char_greater_or_equal_p" #f)
(rewrite 'char>=? 2 2 "C_i_char_greater_or_equal_p" #t)
(rewrite 'char<=? 2 2 "C_u_i_char_less_or_equal_p" #f)
(rewrite 'char<=? 2 2 "C_i_char_less_or_equal_p" #t)
(rewrite '##sys#slot 2 2 "C_slot" #t)		; consider as safe, the primitive is unsafe anyway.
(rewrite '##sys#block-ref 2 2 "C_i_block_ref" #t) ;XXX must be safe for pattern matcher (anymore?)
(rewrite '##sys#size 2 1 "C_block_size" #t)
(rewrite 'fxnot 2 1 "C_fixnum_not" #t)
(rewrite 'fx* 2 2 "C_fixnum_times" #t)
(rewrite 'fx+? 2 2 "C_i_o_fixnum_plus" #t)
(rewrite 'fx-? 2 2 "C_i_o_fixnum_difference" #t)
(rewrite 'fx*? 2 2 "C_i_o_fixnum_times" #t)
(rewrite 'fx/? 2 2 "C_i_o_fixnum_quotient" #t)
(rewrite 'fx= 2 2 "C_eqp" #t)
(rewrite 'fx> 2 2 "C_fixnum_greaterp" #t)
(rewrite 'fx< 2 2 "C_fixnum_lessp" #t)
(rewrite 'fx>= 2 2 "C_fixnum_greater_or_equal_p" #t)
(rewrite 'fx<= 2 2 "C_fixnum_less_or_equal_p" #t)
(rewrite 'fp= 2 2 "C_flonum_equalp" #f)
(rewrite 'fp> 2 2 "C_flonum_greaterp" #f)
(rewrite 'fp< 2 2 "C_flonum_lessp" #f)
(rewrite 'fp>= 2 2 "C_flonum_greater_or_equal_p" #f)
(rewrite 'fp<= 2 2 "C_flonum_less_or_equal_p" #f)
(rewrite 'fxmax 2 2 "C_i_fixnum_max" #t)
(rewrite 'fxmin 2 2 "C_i_fixnum_min" #t)
(rewrite 'fpmax 2 2 "C_i_flonum_max" #f)
(rewrite 'fpmin 2 2 "C_i_flonum_min" #f)
(rewrite 'fxgcd 2 2 "C_i_fixnum_gcd" #t)
(rewrite 'fxlen 2 1 "C_i_fixnum_length" #t)
(rewrite 'char-numeric? 2 1 "C_u_i_char_numericp" #t)
(rewrite 'char-alphabetic? 2 1 "C_u_i_char_alphabeticp" #t)
(rewrite 'char-whitespace? 2 1 "C_u_i_char_whitespacep" #t)
(rewrite 'char-upper-case? 2 1 "C_u_i_char_upper_casep" #t)
(rewrite 'char-lower-case? 2 1 "C_u_i_char_lower_casep" #t)
(rewrite 'char-upcase 2 1 "C_u_i_char_upcase" #t)
(rewrite 'char-downcase 2 1 "C_u_i_char_downcase" #t)
(rewrite 'list-tail 2 2 "C_i_list_tail" #t)
(rewrite '##sys#structure? 2 2 "C_i_structurep" #t)
(rewrite '##sys#bytevector? 2 2 "C_bytevectorp" #t)
(rewrite 'chicken.lolevel#block-ref 2 2 "C_slot" #f)	; ok to be unsafe, lolevel is anyway
(rewrite 'chicken.lolevel#number-of-slots 2 1 "C_block_size" #f)

(rewrite 'assv 14 'fixnum 2 "C_i_assq" "C_u_i_assq")
(rewrite 'assv 2 2 "C_i_assv" #t)
(rewrite 'memv 14 'fixnum 2 "C_i_memq" "C_u_i_memq")
(rewrite 'memv 2 2 "C_i_memv" #t)
(rewrite 'assq 17 2 "C_i_assq" "C_u_i_assq")
(rewrite 'memq 17 2 "C_i_memq" "C_u_i_memq")
(rewrite 'assoc 2 2 "C_i_assoc" #t)
(rewrite 'member 2 2 "C_i_member" #t)

(rewrite 'set-car! 4 '##sys#setslot 0)
(rewrite 'set-cdr! 4 '##sys#setslot 1)
(rewrite 'set-car! 17 2 "C_i_set_car" "C_u_i_set_car")
(rewrite 'set-cdr! 17 2 "C_i_set_cdr" "C_u_i_set_cdr")

(rewrite 'abs 14 'fixnum 1 "C_fixnum_abs" "C_fixnum_abs")

(rewrite 'bitwise-and 21 -1 "C_fixnum_and" "C_u_fixnum_and" "C_s_a_i_bitwise_and" 6)
(rewrite 'bitwise-xor 21 0 "C_fixnum_xor" "C_fixnum_xor" "C_s_a_i_bitwise_xor" 6)
(rewrite 'bitwise-ior 21 0 "C_fixnum_or" "C_u_fixnum_or" "C_s_a_i_bitwise_ior" 6)

(rewrite 'bitwise-not 22 1 "C_s_a_i_bitwise_not" #t 6 "C_fixnum_not")

(rewrite 'fp+ 16 2 "C_a_i_flonum_plus" #f words-per-flonum)
(rewrite 'fp- 16 2 "C_a_i_flonum_difference" #f words-per-flonum)
(rewrite 'fp* 16 2 "C_a_i_flonum_times" #f words-per-flonum)
(rewrite 'fp/ 16 2 "C_a_i_flonum_quotient" #f words-per-flonum)
(rewrite 'fp/? 16 2 "C_a_i_flonum_quotient_checked" #f words-per-flonum)
(rewrite 'fpneg 16 1 "C_a_i_flonum_negate" #f words-per-flonum)
(rewrite 'fpgcd 16 2 "C_a_i_flonum_gcd" #f words-per-flonum)

(rewrite 'zero? 5 "C_eqp" 0 'fixnum)
(rewrite 'zero? 2 1 "C_i_zerop" #t)
(rewrite 'zero? 2 1 "C_u_i_zerop" #f)
(rewrite 'positive? 5 "C_fixnum_greaterp" 0 'fixnum)
(rewrite 'positive? 5 "C_flonum_greaterp" 0 'flonum)
(rewrite 'positive? 2 1 "C_i_positivep" #t)
(rewrite 'positive? 2 1 "C_u_i_positivep" #f)
(rewrite 'negative? 5 "C_fixnum_lessp" 0 'fixnum)
(rewrite 'negative? 5 "C_flonum_lessp" 0 'flonum)
(rewrite 'negative? 2 1 "C_i_negativep" #t)
(rewrite 'negative? 2 1 "C_u_i_negativep" #f)

(rewrite 'vector-length 6 "C_fix" "C_header_size" #f)
(rewrite 'string-length 6 "C_fix" "C_header_size" #f)
(rewrite 'char->integer 6 "C_fix" "C_character_code" #t)
(rewrite 'integer->char 6 "C_make_character" "C_unfix" #t)

(rewrite 'vector-length 2 1 "C_i_vector_length" #t)
(rewrite '##sys#vector-length 2 1 "C_i_vector_length" #t)
(rewrite 'string-length 2 1 "C_i_string_length" #t)

(rewrite '##sys#check-exact 2 1 "C_i_check_exact" #t)
(rewrite '##sys#check-fixnum 2 1 "C_i_check_fixnum" #t)
(rewrite '##sys#check-number 2 1 "C_i_check_number" #t)
(rewrite '##sys#check-list 2 1 "C_i_check_list" #t)
(rewrite '##sys#check-pair 2 1 "C_i_check_pair" #t)
(rewrite '##sys#check-boolean 2 1 "C_i_check_boolean" #t)
(rewrite '##sys#check-locative 2 1 "C_i_check_locative" #t)
(rewrite '##sys#check-symbol 2 1 "C_i_check_symbol" #t)
(rewrite '##sys#check-string 2 1 "C_i_check_string" #t)
(rewrite '##sys#check-byte-vector 2 1 "C_i_check_bytevector" #t)
(rewrite '##sys#check-vector 2 1 "C_i_check_vector" #t)
(rewrite '##sys#check-structure 2 2 "C_i_check_structure" #t)
(rewrite '##sys#check-char 2 1 "C_i_check_char" #t)
(rewrite '##sys#check-exact 2 2 "C_i_check_exact_2" #t)
(rewrite '##sys#check-fixnum 2 2 "C_i_check_fixnum_2" #t)
(rewrite '##sys#check-number 2 2 "C_i_check_number_2" #t)
(rewrite '##sys#check-list 2 2 "C_i_check_list_2" #t)
(rewrite '##sys#check-pair 2 2 "C_i_check_pair_2" #t)
(rewrite '##sys#check-boolean 2 2 "C_i_check_boolean_2" #t)
(rewrite '##sys#check-locative 2 2 "C_i_check_locative_2" #t)
(rewrite '##sys#check-symbol 2 2 "C_i_check_symbol_2" #t)
(rewrite '##sys#check-string 2 2 "C_i_check_string_2" #t)
(rewrite '##sys#check-byte-vector 2 2 "C_i_check_bytevector_2" #t)
(rewrite '##sys#check-vector 2 2 "C_i_check_vector_2" #t)
(rewrite '##sys#check-structure 2 3 "C_i_check_structure_2" #t)
(rewrite '##sys#check-char 2 2 "C_i_check_char_2" #t)

(rewrite '= 9 "C_eqp" "C_i_equalp" #t #t)
(rewrite '> 9 "C_fixnum_greaterp" "C_flonum_greaterp" #t #f)
(rewrite '< 9 "C_fixnum_lessp" "C_flonum_lessp" #t #f)
(rewrite '>= 9 "C_fixnum_greater_or_equal_p" "C_flonum_greater_or_equal_p" #t #f)
(rewrite '<= 9 "C_fixnum_less_or_equal_p" "C_flonum_less_or_equal_p" #t #f)

(rewrite 'setter 11 1 '##sys#setter #t)
(rewrite 'for-each 11 2 '##sys#for-each #t)
(rewrite 'map 11 2 '##sys#map #t)
(rewrite 'chicken.lolevel#block-set! 11 3 '##sys#setslot #t)
(rewrite '##sys#block-set! 11 3 '##sys#setslot #f)
(rewrite 'chicken.lolevel#make-record-instance 11 #f '##sys#make-structure #f)
(rewrite 'substring 11 3 '##sys#substring #f)
(rewrite 'string-append 11 2 '##sys#string-append #f)
(rewrite 'string->list 11 1 '##sys#string->list #t)
(rewrite 'list->string 11 1 '##sys#list->string #t)

(rewrite 'vector-set! 11 3 '##sys#setslot #f)
(rewrite 'vector-set! 2 3 "C_i_vector_set" #t)

(rewrite 'gcd 12 '##sys#gcd #t 2)
(rewrite 'lcm 12 '##sys#lcm #t 2)
(rewrite 'chicken.data-structures#identity 12 #f #t 1)

(rewrite 'gcd 18 0)
(rewrite 'lcm 18 1)
(rewrite 'list 18 '())

(rewrite '+ 16 2 "C_s_a_i_plus" #t 36)
(rewrite '- 16 2 "C_s_a_i_minus" #t 36)
(rewrite '* 16 2 "C_s_a_i_times" #t 40)
(rewrite 'quotient 16 2 "C_s_a_i_quotient" #t 6)
(rewrite 'remainder 16 2 "C_s_a_i_remainder" #t 6)
(rewrite 'modulo 16 2 "C_s_a_i_modulo" #t 6)

(rewrite '= 17 2 "C_i_nequalp")
(rewrite '> 17 2 "C_i_greaterp")
(rewrite '< 17 2 "C_i_lessp")
(rewrite '>= 17 2 "C_i_greater_or_equalp")
(rewrite '<= 17 2 "C_i_less_or_equalp")

(rewrite '= 13 #f "C_nequalp" #t)
(rewrite '> 13 #f "C_greaterp" #t)
(rewrite '< 13 #f "C_lessp" #t)
(rewrite '>= 13 #f "C_greater_or_equal_p" #t)
(rewrite '<= 13 #f "C_less_or_equal_p" #t)

(rewrite '* 13 #f "C_times" #t)
(rewrite '+ 13 #f "C_plus" #t)
(rewrite '- 13 '(1 . #f) "C_minus" #t)

(rewrite 'number->string 13 '(1 . 2) "C_number_to_string" #t)
(rewrite '##sys#call-with-current-continuation 13 1 "C_call_cc" #t)
(rewrite '##sys#allocate-vector 13 4 "C_allocate_vector" #t)
(rewrite '##sys#ensure-heap-reserve 13 1 "C_ensure_heap_reserve" #t)
(rewrite 'return-to-host 13 0 "C_return_to_host" #t)
(rewrite '##sys#context-switch 13 1 "C_context_switch" #t)
(rewrite '##sys#intern-symbol 13 1 "C_string_to_symbol" #t)
(rewrite '##sys#make-symbol 13 1 "C_make_symbol" #t)

(rewrite 'even? 14 'fixnum 1 "C_i_fixnumevenp" "C_i_fixnumevenp")
(rewrite 'odd? 14 'fixnum 1 "C_i_fixnumoddp" "C_i_fixnumoddp")
(rewrite 'remainder 14 'fixnum 2 "C_fixnum_modulo" "C_fixnum_modulo")

(rewrite 'even? 17 1 "C_i_evenp" "C_u_i_evenp")
(rewrite 'odd? 17 1 "C_i_oddp" "C_u_i_oddp")

(rewrite 'fxodd? 2 1 "C_i_fixnumoddp" #t)
(rewrite 'fxeven? 2 1 "C_i_fixnumevenp" #t)

(rewrite 'floor 15 'flonum 'fixnum 'fpfloor #f)
(rewrite 'ceiling 15 'flonum 'fixnum 'fpceiling #f)
(rewrite 'truncate 15 'flonum 'fixnum 'fptruncate #f)

(rewrite 'fpsin 16 1 "C_a_i_flonum_sin" #f words-per-flonum)
(rewrite 'fpcos 16 1 "C_a_i_flonum_cos" #f words-per-flonum)
(rewrite 'fptan 16 1 "C_a_i_flonum_tan" #f words-per-flonum)
(rewrite 'fpasin 16 1 "C_a_i_flonum_asin" #f words-per-flonum)
(rewrite 'fpacos 16 1 "C_a_i_flonum_acos" #f words-per-flonum)
(rewrite 'fpatan 16 1 "C_a_i_flonum_atan" #f words-per-flonum)
(rewrite 'fpatan2 16 2 "C_a_i_flonum_atan2" #f words-per-flonum)
(rewrite 'fpexp 16 1 "C_a_i_flonum_exp" #f words-per-flonum)
(rewrite 'fpexpt 16 2 "C_a_i_flonum_expt" #f words-per-flonum)
(rewrite 'fplog 16 1 "C_a_i_flonum_log" #f words-per-flonum)
(rewrite 'fpsqrt 16 1 "C_a_i_flonum_sqrt" #f words-per-flonum)
(rewrite 'fpabs 16 1 "C_a_i_flonum_abs" #f words-per-flonum)
(rewrite 'fptruncate 16 1 "C_a_i_flonum_truncate" #f words-per-flonum)
(rewrite 'fpround 16 1 "C_a_i_flonum_round" #f words-per-flonum)
(rewrite 'fpceiling 16 1 "C_a_i_flonum_ceiling" #f words-per-flonum)
(rewrite 'fpround 16 1 "C_a_i_flonum_floor" #f words-per-flonum)

(rewrite 'cons 16 2 "C_a_i_cons" #t 3)
(rewrite '##sys#cons 16 2 "C_a_i_cons" #t 3)
(rewrite 'list 16 #f "C_a_i_list" #t '(0 3) #t)
(rewrite '##sys#list 16 #f "C_a_i_list" #t '(0 3))
(rewrite 'vector 16 #f "C_a_i_vector" #t #t #t)
(rewrite '##sys#vector 16 #f "C_a_i_vector" #t #t)
(rewrite '##sys#make-structure 16 #f "C_a_i_record" #t #t #t)
(rewrite 'string 16 #f "C_a_i_string" #t #t) ; the last #t is actually too much, but we don't care
(rewrite 'chicken.lolevel#address->pointer 16 1 "C_a_i_address_to_pointer" #f 2)
(rewrite 'chicken.lolevel#pointer->address 16 1 "C_a_i_pointer_to_address" #f words-per-flonum)
(rewrite 'chicken.lolevel#pointer+ 16 2 "C_a_u_i_pointer_inc" #f 2)

(rewrite 'chicken.lolevel#pointer-u8-ref 2 1 "C_u_i_pointer_u8_ref" #f)
(rewrite 'chicken.lolevel#pointer-s8-ref 2 1 "C_u_i_pointer_s8_ref" #f)
(rewrite 'chicken.lolevel#pointer-u16-ref 2 1 "C_u_i_pointer_u16_ref" #f)
(rewrite 'chicken.lolevel#pointer-s16-ref 2 1 "C_u_i_pointer_s16_ref" #f)
(rewrite 'chicken.lolevel#pointer-u8-set! 2 2 "C_u_i_pointer_u8_set" #f)
(rewrite 'chicken.lolevel#pointer-s8-set! 2 2 "C_u_i_pointer_s8_set" #f)
(rewrite 'chicken.lolevel#pointer-u16-set! 2 2 "C_u_i_pointer_u16_set" #f)
(rewrite 'chicken.lolevel#pointer-s16-set! 2 2 "C_u_i_pointer_s16_set" #f)
(rewrite 'chicken.lolevel#pointer-u32-set! 2 2 "C_u_i_pointer_u32_set" #f)
(rewrite 'chicken.lolevel#pointer-s32-set! 2 2 "C_u_i_pointer_s32_set" #f)
(rewrite 'chicken.lolevel#pointer-f32-set! 2 2 "C_u_i_pointer_f32_set" #f)
(rewrite 'chicken.lolevel#pointer-f64-set! 2 2 "C_u_i_pointer_f64_set" #f)

(rewrite 'chicken.lolevel#pointer-u32-ref 16 1 "C_a_u_i_pointer_u32_ref" #f words-per-flonum)
(rewrite 'chicken.lolevel#pointer-s32-ref 16 1 "C_a_u_i_pointer_s32_ref" #f words-per-flonum)
(rewrite 'chicken.lolevel#pointer-f32-ref 16 1 "C_a_u_i_pointer_f32_ref" #f words-per-flonum)
(rewrite 'chicken.lolevel#pointer-f64-ref 16 1 "C_a_u_i_pointer_f64_ref" #f words-per-flonum)

(rewrite
 '##sys#setslot 8
 (lambda (db classargs cont callargs)
   ;; (##sys#setslot <x> <y> <immediate>) -> (##core#inline "C_i_set_i_slot" <x> <y> <i>)
   ;; (##sys#setslot <x> <y> <z>) -> (##core#inline "C_i_setslot" <x> <y> <z>)
   (and (= (length callargs) 3)
	(make-node 
	 '##core#call (list #t)
	 (list cont
	       (make-node
		'##core#inline
		(let ([val (third callargs)])
		  (if (and (eq? 'quote (node-class val))
			   (immediate? (first (node-parameters val))) ) 
		      '("C_i_set_i_slot")
		      '("C_i_setslot") ) )
		callargs) ) ) ) ) )

(rewrite 'fx+ 17 2 "C_fixnum_plus" "C_u_fixnum_plus")
(rewrite 'fx- 17 2 "C_fixnum_difference" "C_u_fixnum_difference")
(rewrite 'fxshl 17 2 "C_fixnum_shift_left")
(rewrite 'fxshr 17 2 "C_fixnum_shift_right")
(rewrite 'fxneg 17 1 "C_fixnum_negate" "C_u_fixnum_negate")
(rewrite 'fxxor 17 2 "C_fixnum_xor" "C_fixnum_xor")
(rewrite 'fxand 17 2 "C_fixnum_and" "C_u_fixnum_and")
(rewrite 'fxior 17 2 "C_fixnum_or" "C_u_fixnum_or")
(rewrite 'fx/ 17 2 "C_fixnum_divide" "C_u_fixnum_divide")
(rewrite 'fxmod 17 2 "C_fixnum_modulo" "C_u_fixnum_modulo")
(rewrite 'fxrem 17 2 "C_i_fixnum_remainder_checked")

(rewrite
 'arithmetic-shift 8
 (lambda (db classargs cont callargs)
   ;; (arithmetic-shift <x> <-int>)
   ;;           -> (##core#inline "C_fixnum_shift_right" <x> -<int>)
   ;; (arithmetic-shift <x> <+int>)
   ;;           -> (##core#inline "C_fixnum_shift_left" <x> <int>)
   ;; _ -> (##core#inline "C_a_i_arithmetic_shift" <x> <y>)
   ;;
   ;; not in fixnum-mode:
   ;; _ -> (##core#inline_allocate ("C_s_a_i_arithmetic_shift" 6) <x> <y>)
   (and (= 2 (length callargs))
	(let ((val (second callargs)))
	  (make-node
	   '##core#call (list #t)
	   (list cont
		 (or (and-let* (((eq? 'quote (node-class val)))
				((eq? number-type 'fixnum))
				(n (first (node-parameters val)))
				((and (fixnum? n) (not (big-fixnum? n)))) )
		       (if (negative? n)
			   (make-node
			    '##core#inline '("C_fixnum_shift_right")
			    (list (first callargs) (qnode (- n))) )
			   (make-node
			    '##core#inline '("C_fixnum_shift_left")
			    (list (first callargs) val) ) ) )
		     (if (eq? number-type 'fixnum)
			 (make-node '##core#inline
				    '("C_i_fixnum_arithmetic_shift") callargs)
			 (make-node '##core#inline_allocate
				    (list "C_s_a_i_arithmetic_shift" 6)
				    callargs) ) ) ) ) ) ) ) )

(rewrite '##sys#byte 17 2 "C_subbyte")
(rewrite '##sys#setbyte 17 3 "C_setbyte")
(rewrite '##sys#peek-fixnum 17 2 "C_peek_fixnum")
(rewrite '##sys#peek-byte 17 2 "C_peek_byte")
(rewrite 'chicken.lolevel#pointer->object 17 2 "C_pointer_to_object")
(rewrite '##sys#setislot 17 3 "C_i_set_i_slot")
(rewrite '##sys#poke-integer 17 3 "C_poke_integer")
(rewrite '##sys#poke-double 17 3 "C_poke_double")
(rewrite 'string=? 17 2 "C_i_string_equal_p" "C_u_i_string_equal_p")
(rewrite 'string-ci=? 17 2 "C_i_string_ci_equal_p")
(rewrite '##sys#fudge 17 1 "C_fudge")
(rewrite '##sys#permanent? 17 1 "C_permanentp")
(rewrite '##sys#null-pointer? 17 1 "C_null_pointerp" "C_null_pointerp")
(rewrite '##sys#immediate? 17 1 "C_immp")
(rewrite 'chicken.lolevel#locative->object 17 1 "C_i_locative_to_object")
(rewrite 'chicken.lolevel#locative-set! 17 2 "C_i_locative_set")
(rewrite '##sys#foreign-fixnum-argument 17 1 "C_i_foreign_fixnum_argumentp")
(rewrite '##sys#foreign-char-argument 17 1 "C_i_foreign_char_argumentp")
(rewrite '##sys#foreign-flonum-argument 17 1 "C_i_foreign_flonum_argumentp")
(rewrite '##sys#foreign-block-argument 17 1 "C_i_foreign_block_argumentp")
(rewrite '##sys#foreign-struct-wrapper-argument 17 2 "C_i_foreign_struct_wrapper_argumentp")
(rewrite '##sys#foreign-string-argument 17 1 "C_i_foreign_string_argumentp")
(rewrite '##sys#foreign-pointer-argument 17 1 "C_i_foreign_pointer_argumentp")
(rewrite '##sys#foreign-ranged-integer-argument 17 2 "C_i_foreign_ranged_integer_argumentp")
(rewrite '##sys#foreign-unsigned-ranged-integer-argument 17 2 "C_i_foreign_unsigned_ranged_integer_argumentp")

(rewrite 'blob-size 2 1 "C_block_size" #f)

;; TODO: Move this stuff to types.db
(rewrite 'srfi-4#u8vector-ref 2 2 "C_u_i_u8vector_ref" #f)
(rewrite 'srfi-4#s8vector-ref 2 2 "C_u_i_s8vector_ref" #f)
(rewrite 'srfi-4#u16vector-ref 2 2 "C_u_i_u16vector_ref" #f)
(rewrite 'srfi-4#s16vector-ref 2 2 "C_u_i_s16vector_ref" #f)

(rewrite 'srfi-4#f32vector-ref 16 2 "C_a_u_i_f32vector_ref" #f words-per-flonum)
(rewrite 'srfi-4#f64vector-ref 16 2 "C_a_u_i_f64vector_ref" #f words-per-flonum)

(rewrite 'srfi-4#u8vector-set! 2 3 "C_u_i_u8vector_set" #f)
(rewrite 'srfi-4#s8vector-set! 2 3 "C_u_i_s8vector_set" #f)
(rewrite 'srfi-4#u16vector-set! 2 3 "C_u_i_u16vector_set" #f)
(rewrite 'srfi-4#s16vector-set! 2 3 "C_u_i_s16vector_set" #f)
(rewrite 'srfi-4#u32vector-set! 2 3 "C_u_i_u32vector_set" #f)
(rewrite 'srfi-4#s32vector-set! 2 3 "C_u_i_s32vector_set" #f)
(rewrite 'srfi-4#u64vector-set! 2 3 "C_u_i_u32vector_set" #f)
(rewrite 'srfi-4#s64vector-set! 2 3 "C_u_i_s32vector_set" #f)
(rewrite 'srfi-4#f32vector-set! 2 3 "C_u_i_f32vector_set" #f)
(rewrite 'srfi-4#f64vector-set! 2 3 "C_u_i_f64vector_set" #f)

(rewrite 'srfi-4#u8vector-length 2 1 "C_u_i_8vector_length" #f)
(rewrite 'srfi-4#s8vector-length 2 1 "C_u_i_8vector_length" #f)
(rewrite 'srfi-4#u16vector-length 2 1 "C_u_i_16vector_length" #f)
(rewrite 'srfi-4#s16vector-length 2 1 "C_u_i_16vector_length" #f)
(rewrite 'srfi-4#u32vector-length 2 1 "C_u_i_32vector_length" #f)
(rewrite 'srfi-4#s32vector-length 2 1 "C_u_i_32vector_length" #f)
(rewrite 'srfi-4#u64vector-length 2 1 "C_u_i_64vector_length" #f)
(rewrite 'srfi-4#s64vector-length 2 1 "C_u_i_64vector_length" #f)
(rewrite 'srfi-4#f32vector-length 2 1 "C_u_i_32vector_length" #f)
(rewrite 'srfi-4#f64vector-length 2 1 "C_u_i_64vector_length" #f)

(rewrite 'chicken.data-structures#atom? 17 1 "C_i_not_pair_p")

(rewrite 'srfi-4#u8vector->blob/shared 7 1 "C_slot" 1 #f)
(rewrite 'srfi-4#s8vector->blob/shared 7 1 "C_slot" 1 #f)
(rewrite 'srfi-4#u16vector->blob/shared 7 1 "C_slot" 1 #f)
(rewrite 'srfi-4#s16vector->blob/shared 7 1 "C_slot" 1 #f)
(rewrite 'srfi-4#u32vector->blob/shared 7 1 "C_slot" 1 #f)
(rewrite 'srfi-4#s32vector->blob/shared 7 1 "C_slot" 1 #f)
(rewrite 'srfi-4#u64vector->blob/shared 7 1 "C_slot" 1 #f)
(rewrite 'srfi-4#s64vector->blob/shared 7 1 "C_slot" 1 #f)
(rewrite 'srfi-4#f32vector->blob/shared 7 1 "C_slot" 1 #f)
(rewrite 'srfi-4#f64vector->blob/shared 7 1 "C_slot" 1 #f)

(let ()
  (define (rewrite-make-vector db classargs cont callargs)
    ;; (make-vector '<n> [<x>]) -> (let ((<tmp> <x>)) (##core#inline_allocate ("C_a_i_vector" <n>+1) '<n> <tmp>))
    ;; - <n> should be less or equal to 32.
    (let ([argc (length callargs)])
      (and (pair? callargs)
	   (let ([n (first callargs)])
	     (and (eq? 'quote (node-class n))
		  (let ([tmp (gensym)]
			[c (first (node-parameters n))] )
		    (and (fixnum? c)
			 (<= 0 c 32)
			 (let ([val (if (pair? (cdr callargs))
					(second callargs)
					(make-node '##core#undefined '() '()) ) ] )
			   (make-node
			    'let
			    (list tmp)
			    (list val
				  (make-node
				   '##core#call (list #t)
				   (list cont
					 (make-node
					  '##core#inline_allocate 
					  (list "C_a_i_vector" (add1 c))
					  (list-tabulate c (lambda (i) (varnode tmp)) ) ) ) ) ) ) ) ) ) ) ) ) ) )
  (rewrite 'make-vector 8 rewrite-make-vector)
  (rewrite '##sys#make-vector 8 rewrite-make-vector) )

(let ()
  (define (rewrite-call/cc db classargs cont callargs)
    ;; (call/cc <var>), <var> = (lambda (kont k) ... k is never used ...) -> (<var> #f)
    (and (= 1 (length callargs))
	 (let ((val (first callargs)))
	   (and (eq? '##core#variable (node-class val))
		(and-let* ((proc (db-get db (first (node-parameters val)) 'value))
			   ((eq? '##core#lambda (node-class proc))) )
		  (let ((llist (third (node-parameters proc))))
		    (##sys#decompose-lambda-list 
		     llist
		     (lambda (vars argc rest)
		       (and (= argc 2)
			    (let ((var (or rest (second llist))))
			      (and (not (db-get db var 'references))
				   (not (db-get db var 'assigned)) 
				   (not (db-get db var 'inline-transient))
				   (make-node
				    '##core#call (list #t)
				    (list val cont (qnode #f)) ) ) ) ) ) ) ) ) ) ) ) )
  (rewrite 'call-with-current-continuation 8 rewrite-call/cc)
  (rewrite 'call/cc 8 rewrite-call/cc) )

(declare (hide setter-map))

(define setter-map
  '((car . set-car!)
    (cdr . set-cdr!)
    (string-ref . string-set!)
    (vector-ref . vector-set!)
    (srfi-4#u8vector-ref . srfi-4#u8vector-set!)
    (srfi-4#s8vector-ref . srfi-4#s8vector-set!)
    (srfi-4#u16vector-ref . srfi-4#u16vector-set!)
    (srfi-4#s16vector-ref . srfi-4#s16vector-set!)
    (srfi-4#u32vector-ref . srfi-4#u32vector-set!)
    (srfi-4#s32vector-ref . srfi-4#s32vector-set!)
    (srfi-4#u64vector-ref . srfi-4#u64vector-set!)
    (srfi-4#s64vector-ref . srfi-4#s64vector-set!)
    (srfi-4#f32vector-ref . srfi-4#f32vector-set!)
    (srfi-4#f64vector-ref . srfi-4#f64vector-set!)
    (chicken.lolevel#block-ref . chicken.lolevel#block-set!)
    (chicken.lolevel#locative-ref . chicken.lolevel#locative-set!)
    (chicken.lolevel#pointer-u8-ref . chicken.lolevel#pointer-u8-set!)
    (chicken.lolevel#pointer-s8-ref . chicken.lolevel#pointer-s8-set!)
    (chicken.lolevel#pointer-u16-ref . chicken.lolevel#pointer-u16-set!)
    (chicken.lolevel#pointer-s16-ref . chicken.lolevel#pointer-s16-set!)
    (chicken.lolevel#pointer-u32-ref . chicken.lolevel#pointer-u32-set!)
    (chicken.lolevel#pointer-s32-ref . chicken.lolevel#pointer-s32-set!)
    (chicken.lolevel#pointer-f32-ref . chicken.lolevel#pointer-f32-set!)
    (chicken.lolevel#pointer-f64-ref . chicken.lolevel#pointer-f64-set!)))

(rewrite
 '##sys#setter 8
 (lambda (db classargs cont callargs)
   ;; (setter <known-getter>) -> <known-setter>
   (and (= 1 (length callargs))
	(let ((arg (car callargs)))
	  (and (eq? '##core#variable (node-class arg))
	       (let ((sym (car (node-parameters arg))))
		 (and (intrinsic? sym)
		      (and-let* ((a (assq sym setter-map)))
			(make-node
			 '##core#call (list #t)
			 (list cont (varnode (cdr a))) ) ) ) ) ) ) ) ) )
			       
(rewrite 'void 3 '##sys#undefined-value 0)
(rewrite '##sys#void 3 '##sys#undefined-value #f)
(rewrite 'current-thread 3 '##sys#current-thread 0)
(rewrite 'current-input-port 3 '##sys#standard-input 0)
(rewrite 'current-output-port 3 '##sys#standard-output 0)
(rewrite 'current-error-port 3 '##sys#standard-error 0)

(rewrite
 'chicken.data-structures#any? 8
 (lambda (db classargs cont callargs) 
   (and (= 1 (length callargs))
	(let ((arg (car callargs)))
	  (make-node
	   '##core#call (list #t) 
	   (list cont
		 (if (and (eq? '##core#variable (node-class arg))
			  (not (db-get db (car (node-parameters arg)) 'global)) )
		     (qnode #t)
		     (make-node 
		      '##core#inline '("C_anyp")
		      (list arg)) ) ) ) ) ) ) )

(rewrite
 'bit-set? 8
 (lambda (db classargs cont callargs)
   (and (= 2 (length callargs))
	(make-node
	 '##core#call (list #t)
	 (list cont
	       (make-node
		'##core#inline 
		(list (if (eq? number-type 'fixnum) "C_u_i_bit_setp" "C_i_bit_setp"))
		callargs) ) ) ) ) )

(rewrite
 'integer-length 8
 (lambda (db classargs cont callargs)
   (and (= 1 (length callargs))
	(make-node
	 '##core#call (list #t)
	 (list cont
	       (make-node
		'##core#inline 
		(list (if (eq? number-type 'fixnum) "C_i_fixnum_length" "C_i_integer_length"))
		callargs) ) ) ) ) )

(rewrite 'read-char 23 0 '##sys#read-char/port '##sys#standard-input)
(rewrite 'write-char 23 1 '##sys#write-char/port '##sys#standard-output)
(rewrite 'chicken.extras#read-string 23 1 '##sys#read-string/port '##sys#standard-input)
(rewrite 'chicken.data-structures#substring=? 23 2 '##sys#substring=? 0 0 #f)
(rewrite 'chicken.data-structures#substring-ci=? 23 2 '##sys#substring-ci=? 0 0 #f)
(rewrite 'chicken.data-structures#substring-index 23 2 '##sys#substring-index 0)
(rewrite 'chicken.data-structures#substring-index-ci 23 2 '##sys#substring-index-ci 0)

(rewrite 'get-keyword 7 2 "C_i_get_keyword" #f #t)
(rewrite '##sys#get-keyword 7 2 "C_i_get_keyword" #f #t)

)
