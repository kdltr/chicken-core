;;;; srfi-4.scm - Homogeneous numeric vectors
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
 (unit srfi-4)
 (disable-interrupts)
 (not inline ##sys#user-print-hook ##sys#number-hash-hook)
 (foreign-declare #<<EOF
#define C_copy_subvector(to, from, start_to, start_from, bytes)   \
  (C_memcpy((C_char *)C_data_pointer(to) + C_unfix(start_to), (C_char *)C_data_pointer(from) + C_unfix(start_from), C_unfix(bytes)), \
    C_SCHEME_UNDEFINED)
EOF
) )

(include "common-declarations.scm")


;;; Helper routines:

(declare (hide ##sys#check-exact-interval))

(define ##sys#check-exact-interval
  (lambda (n from to loc)
    (##sys#check-exact n loc)
    (if (or (##core#inline "C_fixnum_lessp" n from)
	    (##core#inline "C_fixnum_greaterp" n to) )
	(##sys#error loc "numeric value is not in expected range" n from to) ) ) )

(define-inline (check-range i from to loc)
  (##sys#check-exact i loc)
  (unless (and (fx<= from i) (fx< i to))
    (##sys#error-hook
     (foreign-value "C_OUT_OF_RANGE_ERROR" int)
     loc i from to) ) )


;;; Get vector length:

(define (u8vector-length x)
  (##sys#check-structure x 'u8vector 'u8vector-length)
  (##core#inline "C_u_i_8vector_length" x))

(define (s8vector-length x)
  (##sys#check-structure x 's8vector 's8vector-length)
  (##core#inline "C_u_i_8vector_length" x))

(define (u16vector-length x)
  (##sys#check-structure x 'u16vector 'u16vector-length)
  (##core#inline "C_u_i_16vector_length" x))

(define (s16vector-length x)
  (##sys#check-structure x 's16vector 's16vector-length)
  (##core#inline "C_u_i_16vector_length" x))

(define (u32vector-length x)
  (##sys#check-structure x 'u32vector 'u32vector-length)
  (##core#inline "C_u_i_32vector_length" x))

(define (s32vector-length x)
  (##sys#check-structure x 's32vector 's32vector-length)
  (##core#inline "C_u_i_32vector_length" x))

(define (f32vector-length x)
  (##sys#check-structure x 'f32vector 'f32vector-length)
  (##core#inline "C_u_i_32vector_length" x))

(define (f64vector-length x)
  (##sys#check-structure x 'f64vector 'f64vector-length)
  (##core#inline "C_u_i_64vector_length" x))


;;; Safe accessors:

(define (u8vector-set! x i y)
  (##sys#check-structure x 'u8vector 'u8vector-set!)
  (let ((len (##core#inline "C_u_i_8vector_length" x)))
    (##sys#check-exact y 'u8vector-set!)
    (when (fx< y 0)
      (##sys#error 'u8vector-set! "argument may not be negative" y))
    (check-range i 0 len 'u8vector-set!)
    (##core#inline "C_u_i_u8vector_set" x i y)))

(define (s8vector-set! x i y)
  (##sys#check-structure x 's8vector 's8vector-set!)
  (let ((len (##core#inline "C_u_i_8vector_length" x)))
    (##sys#check-exact y 's8vector-set!)
    (check-range i 0 len 's8vector-set!)
    (##core#inline "C_u_i_s8vector_set" x i y)))

(define (u16vector-set! x i y)
  (##sys#check-structure x 'u16vector 'u16vector-set!)
  (let ((len (##core#inline "C_u_i_16vector_length" x)))
    (##sys#check-exact y 'u16vector-set!)
    (when (fx< y 0)
      (##sys#error 'u16vector-set! "argument may not be negative" y))
    (check-range i 0 len 'u16vector-set!)
    (##core#inline "C_u_i_u16vector_set" x i y)))

(define (s16vector-set! x i y)
  (##sys#check-structure x 's16vector 's16vector-set!)
  (let ((len (##core#inline "C_u_i_16vector_length" x)))
    (##sys#check-exact y 's16vector-set!)
    (check-range i 0 len 's16vector-set!)
    (##core#inline "C_u_i_s16vector_set" x i y)))

(define (u32vector-set! x i y)
  (##sys#check-structure x 'u32vector 'u32vector-set!)
  (let ((len (##core#inline "C_u_i_32vector_length" x)))
    (##sys#check-integer y 'u32vector-set!)
    (cond ((negative? y)
	   (##sys#error 'u32vector-set! "argument may not be negative" y) )
	  ((not (##sys#fits-in-unsigned-int? y))
	   (##sys#error 'u32vector-set! "argument exceeds integer range" y) ) )
    (check-range i 0 len 'u32vector-set!)
    (##core#inline "C_u_i_u32vector_set" x i y)))

(define (s32vector-set! x i y)
  (##sys#check-structure x 's32vector 's32vector-set!)
  (let ((len (##core#inline "C_u_i_32vector_length" x)))
    (##sys#check-integer y 's32vector-set!)
    (unless (##sys#fits-in-int? y)
      (##sys#error 's32vector-set! "argument exceeds integer range" y) )
    (check-range i 0 len 's32vector-set!)
    (##core#inline "C_u_i_s32vector_set" x i y)))

(define (f32vector-set! x i y)
  (##sys#check-structure x 'f32vector 'f32vector-set!)
  (let ((len (##core#inline "C_u_i_32vector_length" x)))
    (##sys#check-number y 'f32vector-set!)
    (check-range i 0 len 'f32vector-set!)
    (##core#inline
     "C_u_i_f32vector_set"
     x i 
     (if (##core#inline "C_blockp" y)
	 y
	 (##core#inline_allocate ("C_a_i_fix_to_flo" 4) y)))))

(define (f64vector-set! x i y)
  (##sys#check-structure x 'f64vector 'f64vector-set!)
  (let ((len (##core#inline "C_u_i_64vector_length" x)))
    (##sys#check-number y 'f64vector-set!)
    (check-range i 0 len 'f64vector-set!)
    (##core#inline
     "C_u_i_f64vector_set"
     x i 
     (if (##core#inline "C_blockp" y)
	 y
	 (##core#inline_allocate ("C_a_i_fix_to_flo" 4) y)))))

(define u8vector-ref
  (getter-with-setter
   (lambda (x i)
     (##sys#check-structure x 'u8vector 'u8vector-ref)
     (let ((len (##core#inline "C_u_i_s8vector_length" x)))
       (check-range i 0 len 'u8vector-ref)
       (##core#inline "C_u_i_u8vector_ref" x i)))
   u8vector-set!
   "(u8vector-ref v i)"))

(define s8vector-ref
  (getter-with-setter
   (lambda (x i)
     (##sys#check-structure x 's8vector 's8vector-ref)
     (let ((len (##core#inline "C_u_i_s8vector_length" x)))
       (check-range i 0 len 's8vector-ref)
       (##core#inline "C_u_i_s8vector_ref" x i)))
   u8vector-set!
   "(s8vector-ref v i)"))

(define u16vector-ref
  (getter-with-setter
   (lambda (x i)
     (##sys#check-structure x 'u16vector 'u16vector-ref)
     (let ((len (##core#inline "C_u_i_s16vector_length" x)))
       (check-range i 0 len 'u16vector-ref)
       (##core#inline "C_u_i_u16vector_ref" x i)))
   u16vector-set!
   "(u16vector-ref v i)"))

(define s16vector-ref
  (getter-with-setter
   (lambda (x i)
     (##sys#check-structure x 's16vector 's16vector-ref)
     (let ((len (##core#inline "C_u_i_s16vector_length" x)))
       (check-range i 0 len 's16vector-ref)
       (##core#inline "C_u_i_s16vector_ref" x i)))
   s16vector-set!
   "(s16vector-ref v i)"))
   
(define u32vector-ref
  (getter-with-setter
   (lambda (x i)
     (##sys#check-structure x 'u32vector 'u32vector-ref)
     (let ((len (##core#inline "C_u_i_u32vector_length" x)))
       (check-range i 0 len 'u32vector-ref)
       (##core#inline_allocate ("C_a_u_i_u32vector_ref" 4) x i)))
   u32vector-set!
   "(u32vector-ref v i)"))

(define s32vector-ref
  (getter-with-setter
   (lambda (x i)
     (##sys#check-structure x 's32vector 's32vector-ref)
     (let ((len (##core#inline "C_u_i_s32vector_length" x)))
       (check-range i 0 len 's32vector-ref)
       (##core#inline_allocate ("C_a_u_i_s32vector_ref" 4) x i)))
   s32vector-set!
   "(s32vector-ref v i)"))

(define f32vector-ref
  (getter-with-setter
   (lambda (x i)
     (##sys#check-structure x 'f32vector 'f32vector-ref)
     (let ((len (##core#inline "C_u_i_f32vector_length" x)))
       (check-range i 0 len 'f32vector-ref)
       (##core#inline_allocate ("C_a_u_i_f32vector_ref" 4) x i)))
   f32vector-set!
   "(f32vector-ref v i)"))

(define f64vector-ref
  (getter-with-setter
   (lambda (x i)
     (##sys#check-structure x 'f64vector 'f64vector-ref)
     (let ((len (##core#inline "C_u_i_f64vector_length" x)))
       (check-range i 0 len 'f64vector-ref)
       (##core#inline_allocate ("C_a_u_i_f64vector_ref" 4) x i)))
   f64vector-set!
   "(f64vector-ref v i)"))


;;; Basic constructors:

(let* ([ext-alloc
	(foreign-lambda* scheme-object ([int bytes])
	  "C_word *buf = (C_word *)C_malloc(bytes + sizeof(C_header));"
	  "if(buf == NULL) C_return(C_SCHEME_FALSE);"
	  "C_block_header(buf) = C_make_header(C_BYTEVECTOR_TYPE, bytes);"
	  "C_return(buf);") ]
       [ext-free
	(foreign-lambda* void ([scheme-object bv])
	  "C_free((void *)C_block_item(bv, 1));") ]
       [alloc
	(lambda (loc len ext?)
	  (if ext?
	      (let ([bv (ext-alloc len)])
		(or bv
		    (##sys#error loc "not enough memory - cannot allocate external number vector" len)) )
	      (let ([bv (##sys#allocate-vector len #t #f #t)]) ; this could be made better...
		(##core#inline "C_string_to_bytevector" bv)
		bv) ) ) ] )

  (set! release-number-vector
    (lambda (v)
      (if (and (##sys#generic-structure? v)
	       (memq (##sys#slot v 0)
		     '(u8vector u16vector s8vector s16vector u32vector s32vector f32vector f64vector)) )
	  (ext-free v)
	  (##sys#error 'release-number-vector "bad argument type - not a number vector" v)) ) )

  (set! make-u8vector
    (lambda (len #!optional (init #f)  (ext? #f) (fin? #t))
      (##sys#check-exact len 'make-u8vector)
      (let ((v (##sys#make-structure 'u8vector (alloc 'make-u8vector len ext?))))
	(when (and ext? fin?) (set-finalizer! v ext-free))
	(if (not init)
	    v
	    (begin
	      (##sys#check-exact-interval init 0 #xff 'make-u8vector)
	      (do ((i 0 (##core#inline "C_fixnum_plus" i 1)))
		  ((##core#inline "C_fixnum_greater_or_equal_p" i len) v)
		(##core#inline "C_u_i_u8vector_set" v i init) ) ) ) ) ) )

  (set! make-s8vector
    (lambda (len #!optional (init #f)  (ext? #f) (fin? #t))
      (##sys#check-exact len 'make-s8vector)
      (let ((v (##sys#make-structure 's8vector (alloc 'make-s8vector len ext?))))
	(when (and ext? fin?) (set-finalizer! v ext-free))
	(if (not init)
	    v
	    (begin
	      (##sys#check-exact-interval init -128 127 'make-s8vector)
	      (do ((i 0 (##core#inline "C_fixnum_plus" i 1)))
		  ((##core#inline "C_fixnum_greater_or_equal_p" i len) v)
		(##core#inline "C_u_i_s8vector_set" v i init) ) ) ) ) ) )

  (set! make-u16vector
    (lambda (len #!optional (init #f)  (ext? #f) (fin? #t))
      (##sys#check-exact len 'make-u16vector)
      (let ((v (##sys#make-structure 'u16vector (alloc 'make-u16vector (##core#inline "C_fixnum_shift_left" len 1) ext?))))
	(when (and ext? fin?) (set-finalizer! v ext-free))
	(if (not init)
	    v
	    (begin
	      (##sys#check-exact-interval init 0 #xffff 'make-u16vector)
	      (do ((i 0 (##core#inline "C_fixnum_plus" i 1)))
		  ((##core#inline "C_fixnum_greater_or_equal_p" i len) v)
		(##core#inline "C_u_i_u16vector_set" v i init) ) ) ) ) ) )

  (set! make-s16vector
    (lambda (len #!optional (init #f)  (ext? #f) (fin? #t))
      (##sys#check-exact len 'make-s16vector)
      (let ((v (##sys#make-structure 's16vector (alloc 'make-s16vector (##core#inline "C_fixnum_shift_left" len 1) ext?))))
	(when (and ext? fin?) (set-finalizer! v ext-free))
	(if (not init)
	    v
	    (begin
	      (##sys#check-exact-interval init -32768 32767 'make-s16vector)
	      (do ((i 0 (##core#inline "C_fixnum_plus" i 1)))
		  ((##core#inline "C_fixnum_greater_or_equal_p" i len) v)
		(##core#inline "C_u_i_s16vector_set" v i init) ) ) ) ) ) )

  (set! make-u32vector
    (lambda (len #!optional (init #f)  (ext? #f) (fin? #t))
      (##sys#check-exact len 'make-u32vector)
      (let ((v (##sys#make-structure 'u32vector (alloc 'make-u32vector (##core#inline "C_fixnum_shift_left" len 2) ext?))))
	(when (and ext? fin?) (set-finalizer! v ext-free))
	(if (not init)
	    v
	    (begin
	      (##sys#check-exact init 'make-u32vector)
	      (do ((i 0 (##core#inline "C_fixnum_plus" i 1)))
		  ((##core#inline "C_fixnum_greater_or_equal_p" i len) v)
		(##core#inline "C_u_i_u32vector_set" v i init) ) ) ) ) ) )

  (set! make-s32vector
    (lambda (len #!optional (init #f)  (ext? #f) (fin? #t))
      (##sys#check-exact len 'make-s32vector)
      (let ((v (##sys#make-structure 's32vector (alloc 'make-s32vector (##core#inline "C_fixnum_shift_left" len 2) ext?))))
	(when (and ext? fin?) (set-finalizer! v ext-free))
	(if (not init)
	    v
	    (begin
	      (##sys#check-exact init 'make-s32vector)
	      (do ((i 0 (##core#inline "C_fixnum_plus" i 1)))
		  ((##core#inline "C_fixnum_greater_or_equal_p" i len) v)
		(##core#inline "C_u_i_s32vector_set" v i init) ) ) ) ) ) )

  (set! make-f32vector
    (lambda (len #!optional (init #f)  (ext? #f) (fin? #t))
      (##sys#check-exact len 'make-f32vector)
      (let ((v (##sys#make-structure 'f32vector (alloc 'make-f32vector (##core#inline "C_fixnum_shift_left" len 2) ext?))))
	(when (and ext? fin?) (set-finalizer! v ext-free))
	(if (not init)
	    v
	    (begin
	      (##sys#check-number init 'make-f32vector)
	      (unless (##core#inline "C_blockp" init)
		(set! init (##core#inline_allocate ("C_a_i_fix_to_flo" 4) init)) )
	      (do ((i 0 (##core#inline "C_fixnum_plus" i 1)))
		  ((##core#inline "C_fixnum_greater_or_equal_p" i len) v)
		(##core#inline "C_u_i_f32vector_set" v i init) ) ) ) ) ) )

  (set! make-f64vector
    (lambda (len #!optional (init #f)  (ext? #f) (fin? #t))
      (##sys#check-exact len 'make-f64vector)
      (let ((v (##sys#make-structure
		'f64vector
		(alloc 'make-f64vector (##core#inline "C_fixnum_shift_left" len 3) ext?))))
	(when (and ext? fin?) (set-finalizer! v ext-free))
	(if (not init)
	    v
	    (begin
	      (##sys#check-number init 'make-f64vector)
	      (unless (##core#inline "C_blockp" init)
		(set! init (##core#inline_allocate ("C_a_i_fix_to_flo" 4) init)) )
	      (do ((i 0 (##core#inline "C_fixnum_plus" i 1)))
		  ((##core#inline "C_fixnum_greater_or_equal_p" i len) v)
		(##core#inline "C_u_i_f64vector_set" v i init) ) ) ) ) ) ) )


;;; Creating vectors from a list:

(define-syntax list->NNNvector 
  (er-macro-transformer 
   (lambda (x r c)
     (let* ((tag (##sys#strip-syntax (cadr x)))
	    (tagstr (symbol->string tag))
	    (name (string->symbol (string-append "list->" tagstr)))
	    (make (string->symbol (string-append "make-" tagstr)))
	    (set (string->symbol (string-append tagstr "-set!"))))
       `(define ,name
	  (let ((,make ,make))
	    (lambda (lst)
	      (##sys#check-list lst ',tag)
	      (let* ((n (##core#inline "C_i_length" lst))
		     (v (,make n)) )
		(do ((p lst (##core#inline "C_slot" p 1))
		     (i 0 (##core#inline "C_fixnum_plus" i 1)) )
		    ((##core#inline "C_eqp" p '()) v)
		  (if (and (##core#inline "C_blockp" p) (##core#inline "C_pairp" p))
		      (,set v i (##core#inline "C_slot" p 0))
		      (##sys#error-not-a-proper-list lst) ) ) ) )))))))

(list->NNNvector u8vector)
(list->NNNvector s8vector)
(list->NNNvector u16vector)
(list->NNNvector s16vector)
(list->NNNvector u32vector)
(list->NNNvector s32vector)
(list->NNNvector f32vector)
(list->NNNvector f64vector)


;;; More constructors:

(define u8vector
  (lambda xs (list->u8vector xs)) )

(define s8vector
  (lambda xs (list->s8vector xs)) )

(define u16vector
  (lambda xs (list->u16vector xs)) )

(define s16vector
  (lambda xs (list->s16vector xs)) )

(define u32vector
  (lambda xs (list->u32vector xs)) )

(define s32vector
  (lambda xs (list->s32vector xs)) )

(define f32vector
  (lambda xs (list->f32vector xs)) )

(define f64vector
  (lambda xs (list->f64vector xs)) )


;;; Creating lists from a vector:

(define-syntax NNNvector->list
  (er-macro-transformer
   (lambda (x r c)
     (let* ((tag (##sys#strip-syntax (cadr x)))
	    (alloc? (pair? (cddr x)))
	    (name (string->symbol (string-append (symbol->string tag) "->list"))))
       `(define (,name v)
	  (##sys#check-structure v ',tag ',name)
	  (let ((len (##core#inline ,(conc "C_u_i_" tag "_length") v)))
	    (let loop ((i 0))
	      (if (fx>= i len)
		  '()
		  (cons 
		   ,(if alloc?
			`(##core#inline_allocate (,(conc "C_a_u_i_" tag "_ref") 4) v i)
			`(##core#inline ,(conc "C_u_i_" tag "_ref") v i))
		   (loop (fx+ i 1)) ) ) ) ) ) ) )))

(NNNvector->list u8vector)
(NNNvector->list s8vector)
(NNNvector->list u16vector)
(NNNvector->list s16vector)
(NNNvector->list u32vector #t)
(NNNvector->list s32vector #t)
(NNNvector->list f32vector #t)
(NNNvector->list f64vector #t)


;;; Predicates:

(define (u8vector? x) (##sys#structure? x 'u8vector))
(define (s8vector? x) (##sys#structure? x 's8vector))
(define (u16vector? x) (##sys#structure? x 'u16vector))
(define (s16vector? x) (##sys#structure? x 's16vector))
(define (u32vector? x) (##sys#structure? x 'u32vector))
(define (s32vector? x) (##sys#structure? x 's32vector))
(define (f32vector? x) (##sys#structure? x 'f32vector))
(define (f64vector? x) (##sys#structure? x 'f64vector))


;;; Accessing the packed bytevector:

(declare (hide pack pack-copy unpack unpack-copy))

(define (pack tag loc)
  (lambda (v)
    (##sys#check-structure v tag loc)
    (##sys#slot v 1) ) )

(define (pack-copy tag loc)
  (lambda (v)
    (##sys#check-structure v tag loc)
    (let* ((old (##sys#slot v 1))
	   (new (##sys#make-blob (##sys#size old))))
      (##core#inline "C_copy_block" old new) ) ) )

(define (unpack tag sz loc)
  (lambda (str)
    (##sys#check-byte-vector str loc)
    (let ([len (##sys#size str)])
      (if (or (eq? #t sz)
	      (eq? 0 (##core#inline "C_fixnum_modulo" len sz)))
	  (##sys#make-structure tag str)
	  (##sys#error loc "blob does not have correct size for packing" tag len sz) ) ) ) )

(define (unpack-copy tag sz loc)
  (lambda (str)
    (##sys#check-byte-vector str loc)
    (let* ((len (##sys#size str))
	   (new (##sys#make-blob len)))
      (if (or (eq? #t sz)
	      (eq? 0 (##core#inline "C_fixnum_modulo" len sz)))
	  (##sys#make-structure
	   tag
	   (##core#inline "C_copy_block" str new) )
	  (##sys#error loc "blob does not have correct size for packing" tag len sz) ) ) ) )

(define u8vector->blob/shared (pack 'u8vector 'u8vector->blob/shared))
(define s8vector->blob/shared (pack 's8vector 's8vector->blob/shared))
(define u16vector->blob/shared (pack 'u16vector 'u16vector->blob/shared))
(define s16vector->blob/shared (pack 's16vector 's16vector->blob/shared))
(define u32vector->blob/shared (pack 'u32vector 'u32vector->blob/shared))
(define s32vector->blob/shared (pack 's32vector 's32vector->blob/shared))
(define f32vector->blob/shared (pack 'f32vector 'f32vector->blob/shared))
(define f64vector->blob/shared (pack 'f64vector 'f64vector->blob/shared))

(define u8vector->blob (pack-copy 'u8vector 'u8vector->blob))
(define s8vector->blob (pack-copy 's8vector 's8vector->blob))
(define u16vector->blob (pack-copy 'u16vector 'u16vector->blob))
(define s16vector->blob (pack-copy 's16vector 's16vector->blob))
(define u32vector->blob (pack-copy 'u32vector 'u32vector->blob))
(define s32vector->blob (pack-copy 's32vector 's32vector->blob))
(define f32vector->blob (pack-copy 'f32vector 'f32vector->blob))
(define f64vector->blob (pack-copy 'f64vector 'f64vector->blob))

(define blob->u8vector/shared (unpack 'u8vector #t 'blob->u8vector/shared))
(define blob->s8vector/shared (unpack 's8vector #t 'blob->s8vector/shared))
(define blob->u16vector/shared (unpack 'u16vector 2 'blob->u16vector/shared))
(define blob->s16vector/shared (unpack 's16vector 2 'blob->s16vector/shared))
(define blob->u32vector/shared (unpack 'u32vector 4 'blob->u32vector/shared))
(define blob->s32vector/shared (unpack 's32vector 4 'blob->s32vector/shared))
(define blob->f32vector/shared (unpack 'f32vector 4 'blob->f32vector/shared))
(define blob->f64vector/shared (unpack 'f64vector 8 'blob->f64vector/shared))

(define blob->u8vector (unpack-copy 'u8vector #t 'blob->u8vector))
(define blob->s8vector (unpack-copy 's8vector #t 'blob->s8vector))
(define blob->u16vector (unpack-copy 'u16vector 2 'blob->u16vector))
(define blob->s16vector (unpack-copy 's16vector 2 'blob->s16vector))
(define blob->u32vector (unpack-copy 'u32vector 4 'blob->u32vector))
(define blob->s32vector (unpack-copy 's32vector 4 'blob->s32vector))
(define blob->f32vector (unpack-copy 'f32vector 4 'blob->f32vector))
(define blob->f64vector (unpack-copy 'f64vector 8 'blob->f64vector))


;;; Read syntax:

(set! ##sys#user-read-hook
  (let ([old-hook ##sys#user-read-hook]
	[read read]
	[consers (list 'u8 list->u8vector
		       's8 list->s8vector
		       'u16 list->u16vector
		       's16 list->s16vector
		       'u32 list->u32vector
		       's32 list->s32vector
		       'f32 list->f32vector
		       'f64 list->f64vector) ] )
    (lambda (char port)
      (if (memq char '(#\u #\s #\f #\U #\S #\F))
	  (let* ([x (read port)]
		 [tag (and (symbol? x) x)] )
	    (cond [(or (eq? tag 'f) (eq? tag 'F)) #f]
		  [(memq tag consers) => (lambda (c) ((##sys#slot (##sys#slot c 1) 0) (read port)))]
		  [else (##sys#read-error port "illegal bytevector syntax" tag)] ) )
	  (old-hook char port) ) ) ) )


;;; Printing:

(set! ##sys#user-print-hook
  (let ((old-hook ##sys#user-print-hook))
    (lambda (x readable port)
      (let ((tag (assq (##core#inline "C_slot" x 0)
		       `((u8vector u8 ,u8vector->list)
			 (s8vector s8 ,s8vector->list)
			 (u16vector u16 ,u16vector->list)
			 (s16vector s16 ,s16vector->list)
			 (u32vector u32 ,u32vector->list)
			 (s32vector s32 ,s32vector->list)
			 (f32vector f32 ,f32vector->list)
			 (f64vector f64 ,f64vector->list) ) ) ) )
	(cond (tag
	       (##sys#print #\# #f port)
	       (##sys#print (cadr tag) #f port)
	       (##sys#print ((caddr tag) x) #t port) )
	      (else (old-hook x readable port)) ) ) ) ) )


;;; Subvectors:

(declare (hide subnvector))

(define (subnvector v t es from to loc)
  (##sys#check-structure v t loc)
  (let* ([bv (##sys#slot v 1)]
	 [len (##sys#size bv)]
	 [ilen (##core#inline "C_u_fixnum_divide" len es)] )
    (check-range from 0 (fx+ ilen 1) loc)
    (check-range to 0 (fx+ ilen 1) loc)
    (let* ([size2 (fx* es (fx- to from))]
	   [bv2 (##sys#allocate-vector size2 #t #f #t)] )
      (##core#inline "C_string_to_bytevector" bv2)
      (let ([v (##sys#make-structure t bv2)])
	(##core#inline "C_copy_subvector" bv2 bv 0 (fx* from es) size2)
	v) ) ) )

(define (subu8vector v from to) (subnvector v 'u8vector 1 from to 'subu8vector))
(define (subu16vector v from to) (subnvector v 'u16vector 2 from to 'subu16vector))
(define (subu32vector v from to) (subnvector v 'u32vector 4 from to 'subu32vector))
(define (subs8vector v from to) (subnvector v 's8vector 1 from to 'subs8vector))
(define (subs16vector v from to) (subnvector v 's16vector 2 from to 'subs16vector))
(define (subs32vector v from to) (subnvector v 's32vector 4 from to 'subs32vector))
(define (subf32vector v from to) (subnvector v 'f32vector 4 from to 'subf32vector))
(define (subf64vector v from to) (subnvector v 'f64vector 8 from to 'subf64vector))

(define (write-u8vector v #!optional (port ##sys#standard-output) (from 0)
			(to (u8vector-length v)))
  (##sys#check-structure v 'u8vector 'write-u8vector)
  (##sys#check-output-port port #t 'write-u8vector)
  (do ((i from (fx+ i 1)))
      ((fx>= i to))
    (##sys#write-char-0 
     (integer->char (##core#inline "C_u_i_u8vector_ref" v i))
     port) ) )

(define (read-u8vector! n dest #!optional (port ##sys#standard-input) (start 0))
  (##sys#check-input-port port #t 'read-u8vector!)
  (##sys#check-exact start 'read-u8vector!)
  (##sys#check-structure dest 'u8vector 'read-u8vector!)
  (let ((dest (##sys#slot dest 1)))
    (when n
      (##sys#check-exact n 'read-u8vector!)
      (when (fx> (fx+ start n) (##sys#size dest))
	(set! n (fx- (##sys#size dest) start))))
    (##sys#read-string! n dest port start) ) )

(define read-u8vector
  (let ()
    (define (wrap str n)
      (##sys#make-structure
       'u8vector
       (let ((str2 (##sys#allocate-vector n #t #f #t)))
	 (##core#inline "C_string_to_bytevector" str2)
	 (##core#inline "C_substring_copy" str str2 0 n 0)
	 str2) ) )
    (lambda (#!optional n (p ##sys#standard-input))
      (##sys#check-input-port p #t 'read-u8vector)
      (cond (n (##sys#check-exact n 'read-u8vector)
	       (let* ((str (##sys#allocate-vector n #t #f #t))
		      (n2 (##sys#read-string! n str p 0)) )
		 (##core#inline "C_string_to_bytevector" str)
		 (if (eq? n n2)
		     (##sys#make-structure 'u8vector str)
		     (wrap str n2) ) ) )
	    (else
	     (let ([str (open-output-string)])
	       (let loop ()
		 (let ([c (##sys#read-char-0 p)])
		   (if (eof-object? c)
		       (let* ((s (get-output-string str))
			      (n (##sys#size s)) )
			 (wrap s n) )
		       (begin
			 (##sys#write-char/port c str)
			 (loop)))))))))))

(register-feature! 'srfi-4)
