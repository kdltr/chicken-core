;;;; lfa2.scm - a lightweight "secondary" flow analysis
;
; Copyright (c) 2012-2015, The CHICKEN Team
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


;; This pass does a lightweight flow-analysis on value types, mostly
;; to handle the case where user code performs a type-check followed
;; by inlined accessors (for example when using record structures).
;; Specialization takes place before inlining, so even though we have
;; the type-information, later inlining will still keep the code for
;; checking argument types.


(declare
  (unit lfa2)
  (uses extras support))

(module chicken.compiler.lfa2
    (perform-secondary-flow-analysis)

(import chicken scheme
	chicken.compiler.support
	chicken.format)

(include "tweaks")
(include "mini-srfi-1.scm")

(define d-depth 0)
(define lfa2-debug #t)

(define (d fstr . args)
  (when (and lfa2-debug (##sys#fudge 13))
    (printf "[debug|~a] ~a~?~%" d-depth (make-string d-depth #\space) fstr args)) )

(define dd d)

(define-syntax d (syntax-rules () ((_ . _) (void))))
(define-syntax dd (syntax-rules () ((_ . _) (void))))


;;; Maps checks to types

(define +type-check-map+
  '(("C_i_check_closure" procedure)
    ("C_i_check_exact" fixnum bignum integer ratnum)
    ("C_i_check_inexact" flonum)	; Or an inexact cplxnum...
    ("C_i_check_number" fixnum integer bignum ratnum flonum cplxnum number)
    ("C_i_check_string" string)
    ("C_i_check_bytevector" blob)
    ("C_i_check_symbol" symbol)
    ("C_i_check_list" null pair list)
    ("C_i_check_pair" pair)
    ("C_i_check_locative" locative)
    ("C_i_check_boolean" boolean)
    ("C_i_check_vector" vector)
    ("C_i_check_structure" *struct*)	; special case
    ("C_i_check_char" char)
    ("C_i_check_closure_2" procedure)
    ("C_i_check_exact_2" fixnum bignum integer ratnum)
    ("C_i_check_inexact_2" flonum)	; Or an inexact cplxnum...
    ("C_i_check_number_2" fixnum integer bignum ratnum flonum cplxnum number)
    ("C_i_check_string_2" string)
    ("C_i_check_bytevector_2" blob)
    ("C_i_check_symbol_2" symbol)
    ("C_i_check_list_2" null pair list)
    ("C_i_check_pair_2" pair)
    ("C_i_check_locative_2" locative)
    ("C_i_check_boolean_2" boolean)
    ("C_i_check_vector_2" vector)
    ("C_i_check_structure_2" *struct*)	; special case
    ("C_i_check_char_2" char)))


;; Maps predicates to types
 
(define +predicate-map+
  '(("C_i_closurep" procedure)
    ("C_fixnump" fixnum)
    ("C_bignump" bignum)
    ("C_i_exact_integerp" integer fixnum bignum)
    ("C_i_flonump" flonum)
    ("C_i_numberp" number fixnum integer bignum ratnum flonum cplxnum)
    ("C_i_ratnump" ratnum)
    ("C_i_cplxnump" cplxnum)
    ("C_stringp" string)
    ("C_bytevectorp" blob)
    ("C_i_symbolp" symbol)
    ("C_i_listp" list)
    ("C_i_pairp" pair)
    ("C_locativep" locative)
    ("C_booleanp" boolean)
    ("C_i_vectorp" vector)
    ("C_structurep" struct)
    ("C_i_structurep" *struct*)		; special case
    ("C_charp" char)
    ("C_i_portp" port)
    ("C_i_nullp" null)))


;; Maps constructors to types

(define +constructor-map+
  '(("C_a_i_record1" *struct*)		; special case
    ("C_a_i_record2" *struct*)
    ("C_a_i_record3" *struct*)
    ("C_a_i_record4" *struct*)
    ("C_a_i_record5" *struct*)
    ("C_a_i_record6" *struct*)
    ("C_a_i_record7" *struct*)
    ("C_a_i_record8" *struct*)
    ("C_a_i_record" *struct*)
    ("C_a_i_string" string)
    ("C_a_i_port" port)
    ("C_a_i_vector1" vector)
    ("C_a_i_vector2" vector)
    ("C_a_i_vector3" vector)
    ("C_a_i_vector4" vector)
    ("C_a_i_vector5" vector)
    ("C_a_i_vector6" vector)
    ("C_a_i_vector7" vector)
    ("C_a_i_vector8" vector)
    ("C_a_pair" pair)
    ("C_a_i_bytevector" blob)
    ("C_a_i_make_locative" locative)
    ("C_a_i_vector" vector)
    ("C_a_i_list1" pair)
    ("C_a_i_list2" pair)
    ("C_a_i_list3" pair)
    ("C_a_i_list4" pair)
    ("C_a_i_list5" pair)
    ("C_a_i_list6" pair)
    ("C_a_i_list7" pair)
    ("C_a_i_list8" pair)
    ("C_a_i_cons" pair)
    ("C_a_i_flonum" flonum)
    ("C_a_i_fix_to_flo" flonum)
    ("C_a_i_big_to_flo" flonum)
    ("C_a_i_fix_to_big" bignum)
    ("C_a_i_bignum0" bignum)
    ("C_a_i_bignum1" bignum)
    ("C_a_i_bignum2" bignum)
    ;;XXX there are endless more - is it worth it?
    ))


;;; Walk nodes and perform simplified type-analysis

(define (perform-secondary-flow-analysis node db)
  (let ((stats '()))

    (define (constant-result lit) 
      ;; a simplified variant of the one in scrutinizer.scm
      (cond ((string? lit) 'string)
	    ((symbol? lit) 'symbol)
	    ((fixnum? lit) 'fixnum)
	    ((bignum? lit) 'bignum)
	    ((flonum? lit) 'float)
	    ((ratnum? lit) 'ratnum)
	    ((cplxnum? lit) 'cplxnum)
	    ((exact-integer? lit) 'integer)
	    ((number? lit) 
	     (case number-type 
	       ((fixnum) 'fixnum)
	       ((flonum) 'flonum)
	       (else 'number)))
	    ((boolean? lit) 'boolean)
	    ((null? lit) 'null)
	    ((list? lit) 'list)
	    ((pair? lit) 'pair)
	    ((eof-object? lit) 'eof)
	    ((vector? lit) 'vector)
	    ((and (not (##sys#immediate? lit)) (##sys#generic-structure? lit))
	     `(struct ,(##sys#slot lit 0)))
	    ((char? lit) 'char)
	    (else '*)))

    (define (report elim)
      (cond ((assoc elim stats) =>
	     (lambda (a) (set-cdr! a (add1 (cdr a)))))
	    (else (set! stats (alist-cons elim 1 stats)))))
 
    (define (assigned? var)
      (db-get db var 'assigned))

    (define (droppable? n)
      (or (memq (node-class n) 
		'(quote ##core#undefined ##core#primitive ##core#lambda))
	  (and (eq? '##core#variable (node-class n))
	       (let ((var (first (node-parameters n))))
		 (or (not (db-get db var 'global))
		     (variable-mark var '##compiler#always-bound))))))

    (define (drop! n)
      (node-class-set! n '##core#undefined)
      (node-parameters-set! n '())
      (node-subexpressions-set! n '()))

    (define (extinguish! node rpl)	; replace ##core#inline call
      (report (first (node-parameters node)))
      (let ((subs (node-subexpressions node))
	    (alldropped #t))
	(for-each
	 (lambda (sn)
	   (if (droppable? sn)
	       (drop! sn)
	       (set! alldropped #f)))
	 subs)
	(if alldropped
	    (drop! node)
	    (node-parameters-set!
	     node
	     (list
	      (string-append
	       rpl
	       (case (length (node-subexpressions node))
		 ((1) "1")
		 ((2) "2")
		 ((3) "3")
		 (else (bomb "bad number of arguments to extinguished ##core#inline")))))))))

    (define (vartype v te ae)
      (cond ((assq v te) => cdr)
	    (else
	     (let loop ((ae ae))
	       (cond ((null? ae) '*)
		     ((and (eq? v (cdar ae)) 
			   (assq (caar ae) te) )
		      => cdr)
		     (else (loop (cdr ae))))))))

    (define (walk n te ae)
      (let ((class (node-class n))
	    (params (node-parameters n))
	    (subs (node-subexpressions n)))
	(case class
	  ((##core#variable)
	   (vartype (first params) te ae))
	  ((if ##core#cond) 
	   (let ((tr (walk (first subs) te ae)))
	     (cond ((and (pair? tr) (eq? 'boolean (car tr)))
		    (walk (second subs)
			  (append (second tr) te)
			  ae)
		    (walk (third subs)
			  (append (third tr) te)
			  ae))
		   (else
		    (walk (second subs) te ae)
		    (walk (third subs) te ae)))))
	  ((quote) (constant-result (first params)))
	  ((let)
	   (let* ((val (first subs))
		  (var (first params))
		  (r (walk val te ae)))
	     (walk (second subs)
		   (if (assigned? var) 
		       te
		       (alist-cons var r te))
		   (if (and (eq? '##core#variable (node-class val))
			    (not (assigned? var))
			    (not (assigned? (first (node-parameters val)))))
		       (let ((var2 (first (node-parameters val))))
			 (alist-cons var var2 (alist-cons var2 var ae)))
		       ae))))
	  ((##core#lambda ##core#direct_lambda)
	   ;; fresh env and we don't bother to create entries in the environment
	   ;; for the llist-bound variables (missing implies type '*)
	   ;;XXX (but we could treat the first arg in non-CPS lambdas as procedure...)
	   (walk (first subs) '() '())
	   'procedure)
	  ((set! ##core#set!)	       ;XXX is ##core#set! still used?
	   (walk (first subs) te ae)
	   'undefined)
	  ((##core#undefined) 'undefined)
	  ((##core#primitive) 'procedure)
	  ((##core#inline ##core#inline_allocate)
	   (for-each (cut walk <> te ae) subs)
	   (cond ((assoc (first params) +type-check-map+) =>
		  (lambda (a)
		    (let ((r1 (walk (first subs) te ae)))
		      (cond (unsafe
			     (extinguish! n "C_i_noop"))
			    ((eq? '*struct* (cadr a))
			     ;; handle known structure type
			     (when (and (pair? r1)
					(eq? 'struct (first r1))
					(eq? 'quote (node-class (second subs))))
			       (let ((st (first (node-parameters (second subs)))))
				 (when (and (symbol? st)
					    (eq? st (second r1)))
				   (extinguish! n "C_i_noop")))))
			    ((and (pair? r1) (eq? 'boolean (car r1)))
			     (when (memq 'boolean (cdr a))
			       (extinguish! n "C_i_noop")))
			    ;; handle other types
			    ((member r1 (cdr a))
			     (extinguish! n "C_i_noop")))
		      '*)))
		 ((assoc (first params) +predicate-map+) =>
		  (lambda (a)
		    (let ((arg (first subs)))
		      (if (eq? '##core#variable (node-class arg))
			  `(boolean
			    ((,(first (node-parameters arg)) 
			      .
			      ,(if (eq? '*struct* (cadr a))
				   (if (eq? 'quote (node-class (second subs)))
				       (let ((st (first
						  (node-parameters
						   (second subs)))))
					 (if (symbol? st)
					     `(struct ,st)
					     'struct))
				       'struct)
				   (cadr a))))
			    ())
			  (let ((r1 (walk (first subs) te ae)))
			    (cond ((eq? '*struct* (cadr a))
				   ;; known structure type
				   (when (and (pair? r1)
					      (eq? 'struct (first r1))
					      (eq? 'quote (node-class (second subs))))
				     (let ((st (first 
						(node-parameters (second subs)))))
				       (when (and (symbol? st)
						  (eq? st (second r1)))
					 (extinguish! n "C_i_true")))))
				  ((and (pair? r1) (eq? 'boolean (car r1)))
				   (when (memq 'boolean (cdr a))
				     (extinguish! n "C_i_true")))
				  ;; other types
				  ((member r1 (cdr a))
				   (extinguish! n "C_i_true")))
			    'boolean)))))
		 ((assoc (first params) +constructor-map+) =>
		  (lambda (a)
		    (let ((arg1 (and (pair? subs) (first subs))))
		      (if (and arg1
			       (eq? '*struct* (cadr a))
			       (eq? 'quote (node-class arg1)))
			  (let ((tag (first (node-parameters arg1))))
			    (if (symbol? tag)
				`(struct ,tag)
				'struct))
			  (cadr a)))))))
	  (else 
	   (for-each (cut walk <> te ae) subs)
	   '*))))

    (walk node '() '())
    (when (pair? stats)
      (with-debugging-output
       '(x o)
       (lambda ()
	 (print "eliminated type checks:")
	 (for-each 
	  (lambda (ss) (printf "  ~a:\t~a~%" (car ss) (cdr ss)))
	  stats))))))
)
