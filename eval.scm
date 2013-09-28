;;;; eval.scm - Interpreter for CHICKEN
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
  (unit eval)
  (uses expand modules)
  (hide pds pdss pxss d) 
  (not inline ##sys#repl-read-hook ##sys#repl-print-hook 
       ##sys#read-prompt-hook ##sys#alias-global-hook ##sys#user-read-hook
       ##sys#syntax-error-hook))

#>
#ifndef C_INSTALL_EGG_HOME
# define C_INSTALL_EGG_HOME    "."
#endif

#ifndef C_INSTALL_SHARE_HOME
# define C_INSTALL_SHARE_HOME NULL
#endif

#ifndef C_BINARY_VERSION
# define C_BINARY_VERSION      0
#endif

#define C_rnd_fix()		(C_fix(rand()))
<#

(include "common-declarations.scm")

(define-syntax d (syntax-rules () ((_ . _) (void))))


(define-foreign-variable install-egg-home c-string "C_INSTALL_EGG_HOME")
(define-foreign-variable installation-home c-string "C_INSTALL_SHARE_HOME")
(define-foreign-variable binary-version int "C_BINARY_VERSION")
(define-foreign-variable install-lib-name c-string "C_INSTALL_LIB_NAME")

(define ##sys#core-library-modules
  '(extras lolevel utils files tcp irregex posix srfi-1 srfi-4 srfi-13 
	   srfi-14 srfi-18 srfi-69 data-structures ports))

(define ##sys#core-syntax-modules
  '(chicken-syntax chicken-ffi-syntax))

(define ##sys#explicit-library-modules '())

(define default-dynamic-load-libraries
  `(,(string-append "lib" install-lib-name)))
(define-constant cygwin-default-dynamic-load-libraries '("cygchicken-0"))
(define-constant macosx-load-library-extension ".dylib")
(define-constant windows-load-library-extension ".dll")
(define-constant hppa-load-library-extension ".sl")
(define-constant default-load-library-extension ".so")
(define-constant environment-table-size 301)
(define-constant source-file-extension ".scm")
(define-constant setup-file-extension "setup-info")
(define-constant repository-environment-variable "CHICKEN_REPOSITORY")
(define-constant prefix-environment-variable "CHICKEN_PREFIX")

; these are actually in unit extras, but that is used by default
; srfi-12 in unit library
; srfi-98 partially in unit posix

(define-constant builtin-features
  '(chicken srfi-2 srfi-6 srfi-10 srfi-12 srfi-23 srfi-28 srfi-30 srfi-39 
	    srfi-55 srfi-88 srfi-98) )

(define-constant builtin-features/compiled
  '(srfi-8 srfi-9 srfi-11 srfi-15 srfi-16 srfi-17 srfi-26) )

(define ##sys#chicken-prefix
  (let ((prefix (and-let* ((p (get-environment-variable prefix-environment-variable)))
		  (##sys#string-append 
		   p
		   (if (memq (string-ref p (fx- (##sys#size p) 1)) '(#\\ #\/)) "" "/")) ) ) )
    (lambda (#!optional dir)
      (and prefix
	   (if dir (##sys#string-append prefix dir) prefix) ) ) ) )
	  

;;; System settings

(define (chicken-home)
  (or (##sys#chicken-prefix "share/chicken")
      installation-home) )


;;; Lo-level hashtable support:

(define ##sys#hash-symbol
  (let ([cache-s #f]
	[cache-h #f]
        ;; NOTE: All low-level hash tables share the same randomization factor
        [rand (##core#inline "C_rnd_fix")] )
    (lambda (s n)
      (if (eq? s cache-s)
	  (##core#inline "C_fixnum_modulo" cache-h n)
          (begin
            (set! cache-s s)
            (set! cache-h (##core#inline "C_u_i_string_hash" (##sys#slot s 1) rand))
            (##core#inline "C_fixnum_modulo" cache-h n))))))

(define (##sys#hash-table-ref ht key)
  (let loop ((bucket (##sys#slot ht (##sys#hash-symbol key (##core#inline "C_block_size" ht)))))
      (and (not (eq? '() bucket))
           (if (eq? key (##sys#slot (##sys#slot bucket 0) 0))
               (##sys#slot (##sys#slot bucket 0) 1)
               (loop (##sys#slot bucket 1))))))

(define (##sys#hash-table-set! ht key val)
  (let* ((k (##sys#hash-symbol key (##core#inline "C_block_size" ht)))
         (ib (##sys#slot ht k)))
      (let loop ((bucket ib))
          (if (eq? '() bucket)
              (##sys#setslot ht k (cons (cons key val) ib))
              (if (eq? key (##sys#slot (##sys#slot bucket 0) 0))
                  (##sys#setslot (##sys#slot bucket 0) 1 val)
                  (loop (##sys#slot bucket 1)))))))

(define (##sys#hash-table-update! ht key updtfunc valufunc)
  (##sys#hash-table-set! ht key (updtfunc (or (##sys#hash-table-ref ht key) (valufunc)))) )

(define (##sys#hash-table-for-each p ht)
  (let ((len (##core#inline "C_block_size" ht)))
    (do ((i 0 (fx+ i 1)))
	((fx>= i len))
      (##sys#for-each (lambda (bucket) (p (##sys#slot bucket 0) (##sys#slot bucket 1)))
		      (##sys#slot ht i) ) ) ) )

(define ##sys#hash-table-location
  (let ([unbound (##sys#slot '##sys#arbitrary-unbound-symbol 0)])
    (lambda (ht key addp)
      (let* ([k (##sys#hash-symbol key (##sys#size ht))]
	     [bucket0 (##sys#slot ht k)] )
	(let loop ([bucket bucket0])
	  (if (null? bucket)
	      (and addp
		   (let ([p (vector key unbound #t)])
		     (##sys#setslot ht k (cons p bucket0))
		     p) )
	      (let ([b (##sys#slot bucket 0)])
		(if (eq? key (##sys#slot b 0))
		    b
		    (loop (##sys#slot bucket 1)) ) ) ) ) ) ) ) )

(define (##sys#hash-table-size ht)
  (let loop ((len (##sys#size ht)) (bkt 0) (size 0))
    (if (fx= bkt len)
        size
        (loop len (fx+ bkt 1) (fx+ size (##sys#length (##sys#slot ht bkt)))))))

;;; Compile lambda to closure:

(define (##sys#eval-decorator p ll h cntr)
  (##sys#decorate-lambda
   p 
   (lambda (x) (and (not (##sys#immediate? x)) (##core#inline "C_lambdainfop" x)))
   (lambda (p i)
     (##sys#setslot 
      p i 
      (##sys#make-lambda-info 
       (let ((o (open-output-string)))
	 (write ll o)
	 (get-output-string o))))
     p) ) )

(define ##sys#unbound-in-eval #f)
(define ##sys#eval-debug-level (make-parameter 1))

(define ##sys#compile-to-closure
  (let ([write write]
	[reverse reverse]
	[with-input-from-file with-input-from-file]
	[unbound (##sys#slot '##sys#arbitrary-unbound-symbol 0)]
	[display display] )
    (lambda (exp env se #!optional cntr evalenv static)

      (define (find-id id se)		; ignores macro bindings
	(cond ((null? se) #f)
	      ((and (eq? id (caar se)) (symbol? (cdar se))) (cdar se))
	      (else (find-id id (cdr se)))))

      (define (rename var se)
	(cond ((find-id var se))
	      ((##sys#get var '##core#macro-alias))
	      (else var)))

      (define (lookup var0 e se)
	(let ((var (rename var0 se)))
	  (d `(LOOKUP/EVAL: ,var0 ,var ,e ,(map (lambda (x) (car x)) se)))
	  (let loop ((envs e) (ei 0))
	    (cond ((null? envs) (values #f var))
		  ((posq var (##sys#slot envs 0)) => (lambda (p) (values ei p)))
		  (else (loop (##sys#slot envs 1) (fx+ ei 1))) ) ) ))

      (define (posq x lst)
	(let loop ((lst lst) (i 0))
	  (cond ((null? lst) #f)
		((eq? x (##sys#slot lst 0)) i)
		(else (loop (##sys#slot lst 1) (fx+ i 1))) ) ) )

      (define (emit-trace-info tf info cntr e v) 
	(when tf
	  (##core#inline 
	   "C_emit_eval_trace_info" 
	   info
	   (##sys#make-structure 'frameinfo cntr e v)
	   ##sys#current-thread) ) )
      
      (define (emit-syntax-trace-info tf info cntr) 
	(when tf
	  (##core#inline
	   "C_emit_syntax_trace_info"
	   info
	   cntr
	   ##sys#current-thread) ) )
	
      (define (decorate p ll h cntr)
	(##sys#eval-decorator p ll h cntr) )

      (define (compile x e h tf cntr se)
	(cond ((keyword? x) (lambda v x))
	      ((symbol? x)
	       (receive (i j) (lookup x e se)
		 (cond ((not i)
			(let ((var (cond ((not (symbol? j)) x) ; syntax?
                                         ((not (assq x se))
                                          (and (not static)
                                               (##sys#alias-global-hook j #f cntr)))
                                         (else (or (##sys#get j '##core#primitive) j)))))
			  (when (and ##sys#unbound-in-eval
				     (or (not var)
					 (not (##sys#symbol-has-toplevel-binding? var))))
			    (set! ##sys#unbound-in-eval
			      (cons (cons var cntr) ##sys#unbound-in-eval)) )
			  (cond ((not var)
				 (lambda (v)
				   (##sys#error "unbound variable" x)))
				(else
				 (lambda v (##core#inline "C_retrieve" var))))))
                      (else
                       (case i
                         ((0) (lambda (v) 
                                (##sys#slot (##sys#slot v 0) j)))
                         ((1) (lambda (v) 
                                (##sys#slot (##sys#slot (##sys#slot v 1) 0) j)))
                         ((2) (lambda (v) 
                                (##sys#slot 
                                 (##sys#slot (##sys#slot (##sys#slot v 1) 1) 0)
                                 j)))
                         ((3) (lambda (v) 
                                (##sys#slot 
                                 (##sys#slot
                                  (##sys#slot (##sys#slot (##sys#slot v 1) 1) 1)
                                  0)
                                 j)))
                         (else
                          (lambda (v)
                            (##sys#slot (##core#inline "C_u_i_list_ref" v i) j))))))))
	      [(##sys#number? x)
	       (case x
		 [(-1) (lambda v -1)]
		 [(0) (lambda v 0)]
		 [(1) (lambda v 1)]
		 [(2) (lambda v 2)]
		 [else (lambda v x)] ) ]
	      [(boolean? x)
	       (if x
		   (lambda v #t)
		   (lambda v #f) ) ]
	      [(or (char? x)
		   (eof-object? x)
		   (string? x) )
	       (lambda v x) ]
	      [(not (pair? x)) 
	       (##sys#syntax-error/context "illegal non-atomic object" x)]
	      [(symbol? (##sys#slot x 0))
	       (emit-syntax-trace-info tf x cntr)
	       (let ((x2 (##sys#expand x se #f)))
		 (d `(EVAL/EXPANDED: ,x2))
		 (if (not (eq? x2 x))
		     (compile x2 e h tf cntr se)
		     (let ((head (rename (##sys#slot x 0) se)))
		       ;; here we did't resolve ##core#primitive, but that is done in compile-call (via 
		       ;; a normal walking of the operator)
		       (case head

			 [(##core#quote)
			  (let* ((c (##sys#strip-syntax (cadr x))))
			    (case c
			      [(-1) (lambda v -1)]
			      [(0) (lambda v 0)]
			      [(1) (lambda v 1)]
			      [(2) (lambda v 2)]
			      [(#t) (lambda v #t)]
			      [(#f) (lambda v #f)]
			      [(()) (lambda v '())]
			      [else (lambda v c)] ) ) ]

			 ((##core#syntax)
			  (let ((c (cadr x)))
			    (lambda v c)))

			 [(##core#check)
			  (compile (cadr x) e h tf cntr se) ]

			 [(##core#immutable)
			  (compile (cadr x) e #f tf cntr se) ]
		   
			 [(##core#undefined) (lambda (v) (##core#undefined))]

			 [(##core#if)
			  (let* ([test (compile (cadr x) e #f tf cntr se)]
				 [cns (compile (caddr x) e #f tf cntr se)]
				 [alt (if (pair? (cdddr x))
					  (compile (cadddr x) e #f tf cntr se)
					  (compile '(##core#undefined) e #f tf cntr se) ) ] )
			    (lambda (v) (if (##core#app test v) (##core#app cns v) (##core#app alt v))) ) ]

			 [(##core#begin ##core#toplevel-begin)
			  (let* ((body (##sys#slot x 1))
				 (len (length body)) )
			    (case len
			      [(0) (compile '(##core#undefined) e #f tf cntr se)]
			      [(1) (compile (##sys#slot body 0) e #f tf cntr se)]
			      [(2) (let* ([x1 (compile (##sys#slot body 0) e #f tf cntr se)]
					  [x2 (compile (cadr body) e #f tf cntr se)] )
				     (lambda (v) (##core#app x1 v) (##core#app x2 v)) ) ]
			      [else
			       (let* ([x1 (compile (##sys#slot body 0) e #f tf cntr se)]
				      [x2 (compile (cadr body) e #f tf cntr se)] 
				      [x3 (compile `(##core#begin ,@(##sys#slot (##sys#slot body 1) 1)) e #f tf cntr se)] )
				 (lambda (v) (##core#app x1 v) (##core#app x2 v) (##core#app x3 v)) ) ] ) ) ]

			 [(##core#set!)
			  (let ((var (cadr x)))
			    (receive (i j) (lookup var e se)
			      (let ((val (compile (caddr x) e var tf cntr se)))
				(cond [(not i)
				       (when ##sys#notices-enabled
					 (and-let* ((a (assq var (##sys#current-environment)))
						    ((symbol? (cdr a))))
					   (##sys#notice "assignment to imported value binding" var)))
				       (let ((var
					      (if (not (assq x se))
						  (and (not static)
						       (##sys#alias-global-hook j #t cntr))
						  (or (##sys#get j '##core#primitive) j))))
					 (if (not var) ; static
					     (lambda (v)
					       (##sys#error 'eval "environment is not mutable" evalenv var))
					     (lambda (v)
					       (##sys#setslot var 0 (##core#app val v))) ) ) ]
				      [(zero? i) (lambda (v) (##sys#setslot (##sys#slot v 0) j (##core#app val v)))]
				      [else
				       (lambda (v)
					 (##sys#setslot
					  (##core#inline "C_u_i_list_ref" v i) j (##core#app val v)) ) ] ) ) ) ) ]

			 [(##core#let)
			  (let* ([bindings (cadr x)]
				 [n (length bindings)] 
				 [vars (map (lambda (x) (car x)) bindings)]
				 (aliases (map gensym vars))
				 [e2 (cons aliases e)]
				 (se2 (##sys#extend-se se vars aliases))
				 [body (##sys#compile-to-closure
					(##sys#canonicalize-body (cddr x) se2 #f)
					e2 se2 cntr evalenv static) ] )
			    (case n
			      [(1) (let ([val (compile (cadar bindings) e (car vars) tf cntr se)])
				     (lambda (v)
				       (##core#app body (cons (vector (##core#app val v)) v)) ) ) ]
			      [(2) (let ([val1 (compile (cadar bindings) e (car vars) tf cntr se)]
					 [val2 (compile (cadadr bindings) e (cadr vars) tf cntr se)] )
				     (lambda (v)
				       (##core#app body (cons (vector (##core#app val1 v) (##core#app val2 v)) v)) ) ) ]
			      [(3) (let* ([val1 (compile (cadar bindings) e (car vars) tf cntr se)]
					  [val2 (compile (cadadr bindings) e (cadr vars) tf cntr se)] 
					  [t (cddr bindings)]
					  [val3 (compile (cadar t) e (caddr vars) tf cntr se)] )
				     (lambda (v)
				       (##core#app 
					body
					(cons (vector (##core#app val1 v) (##core#app val2 v) (##core#app val3 v)) v)) ) ) ]
			      [(4) (let* ([val1 (compile (cadar bindings) e (car vars) tf cntr se)]
					  [val2 (compile (cadadr bindings) e (cadr vars) tf cntr se)] 
					  [t (cddr bindings)]
					  [val3 (compile (cadar t) e (caddr vars) tf cntr se)] 
					  [val4 (compile (cadadr t) e (cadddr vars) tf cntr se)] )
				     (lambda (v)
				       (##core#app 
					body
					(cons (vector (##core#app val1 v)
						      (##core#app val2 v)
						      (##core#app val3 v)
						      (##core#app val4 v))
					      v)) ) ) ]
			      [else
			       (let ([vals (map (lambda (x) (compile (cadr x) e (car x) tf cntr se)) bindings)])
				 (lambda (v)
				   (let ([v2 (##sys#make-vector n)])
				     (do ([i 0 (fx+ i 1)]
					  [vlist vals (##sys#slot vlist 1)] )
					 ((fx>= i n))
				       (##sys#setslot v2 i (##core#app (##sys#slot vlist 0) v)) )
				     (##core#app body (cons v2 v)) ) ) ) ] ) ) ]

			 ((##core#letrec)
			  (let ((bindings (cadr x))
				(body (cddr x)) )
			    (compile
			     `(##core#let
			       ,(##sys#map (lambda (b)
					     (list (car b) '(##core#undefined))) 
					   bindings)
			       ,@(##sys#map (lambda (b)
					      `(##core#set! ,(car b) ,(cadr b))) 
					    bindings)
			       (##core#let () ,@body) )
			     e h tf cntr se)))

			 [(##core#lambda)
			  (##sys#check-syntax 'lambda x '(_ lambda-list . #(_ 1)) #f se)
			  (let* ([llist (cadr x)]
				 [body (cddr x)] 
				 [info (cons (or h '?) llist)] )
			    (when (##sys#extended-lambda-list? llist)
			      (set!-values 
			       (llist body) 
			       (##sys#expand-extended-lambda-list 
				llist body ##sys#syntax-error-hook se) ) ) 
			    (##sys#decompose-lambda-list
			     llist
			     (lambda (vars argc rest)
			       (let* ((aliases (map gensym vars))
				      (se2 (##sys#extend-se se vars aliases))
				      (e2 (cons aliases e))
				      (body 
				       (##sys#compile-to-closure
					(##sys#canonicalize-body body se2 #f)
					e2 se2 (or h cntr) evalenv static) ) )
				 (case argc
				   [(0) (if rest
					    (lambda (v)
					      (decorate
					       (lambda r
						 (##core#app body (cons (vector r) v)))
					       info h cntr) )
					    (lambda (v)
					      (decorate
					       (lambda () (##core#app body (cons #f v)))
					       info h cntr) ) ) ]
				   [(1) (if rest
					    (lambda (v)
					      (decorate
					       (lambda (a1 . r)
						 (##core#app body (cons (vector a1 r) v)))
					       info h cntr) ) 
					    (lambda (v)
					      (decorate 
					       (lambda (a1)
						 (##core#app body (cons (vector a1) v)))
					       info h cntr) ) ) ]
				   [(2) (if rest
					    (lambda (v) 
					      (decorate
					       (lambda (a1 a2 . r)
						 (##core#app body (cons (vector a1 a2 r) v)))
					       info h cntr) )
					    (lambda (v)
					      (decorate
					       (lambda (a1 a2)
						 (##core#app body (cons (vector a1 a2) v)))
					       info h cntr) ) ) ]
				   [(3) (if rest
					    (lambda (v) 
					      (decorate
					       (lambda (a1 a2 a3 . r)
						 (##core#app body (cons (vector a1 a2 a3 r) v)))
					       info h cntr) )
					    (lambda (v)
					      (decorate
					       (lambda (a1 a2 a3)
						 (##core#app body (cons (vector a1 a2 a3) v)))
					       info h cntr) ) ) ]
				   [(4) (if rest
					    (lambda (v)
					      (decorate
					       (lambda (a1 a2 a3 a4 . r)
						 (##core#app body (cons (vector a1 a2 a3 a4 r) v)))
					       info h cntr) )
					    (lambda (v)
					      (decorate
					       (lambda (a1 a2 a3 a4)
						 (##core#app body (##sys#cons (##sys#vector a1 a2 a3 a4) v)))
					       info h cntr) ) ) ]
				   [else 
				    (if rest
					(lambda (v)
					  (decorate
					   (lambda as
					     (##core#app
					      body
					      (##sys#cons (apply ##sys#vector (fudge-argument-list argc as)) v)) )
					   info h cntr) )
					(lambda (v)
					  (decorate
					   (lambda as 
					     (let ([len (length as)])
					       (if (not (fx= len argc))
						   (##sys#error "bad argument count" argc len)
						   (##core#app body (##sys#cons (apply ##sys#vector as) v)))))
					   info h cntr) ) ) ] ) ) ) ) ) ]

			 ((##core#let-syntax)
			  (let ((se2 (append
				      (map (lambda (b)
					     (list
					      (car b)
					      se
					      (##sys#ensure-transformer
					       (##sys#eval/meta (cadr b))
					       (##sys#strip-syntax (car b)))))
					   (cadr x) ) 
				      se) ) )
			    (compile
			     (##sys#canonicalize-body (cddr x) se2 #f)
			     e #f tf cntr se2)))
			       
			 ((##core#letrec-syntax)
			  (let* ((ms (map (lambda (b)
					    (list
					     (car b)
					     #f
					     (##sys#ensure-transformer
					      (##sys#eval/meta (cadr b))
					      (##sys#strip-syntax (car b)))))
					  (cadr x) ) )
				 (se2 (append ms se)) )
			    (for-each 
			     (lambda (sb)
			       (set-car! (cdr sb) se2) )
			     ms) 
			    (compile
			     (##sys#canonicalize-body (cddr x) se2 #f)
			     e #f tf cntr se2)))
			       
			 ((##core#define-syntax)
			  (let* ((var (cadr x))
				 (body (caddr x))
				 (name (rename var se)))
			    (when (and static (not (assq var se)))
			      (##sys#error 'eval "environment is not mutable" evalenv var))
			    (##sys#register-syntax-export 
			     name (##sys#current-module)
			     body)	; not really necessary, it only shouldn't be #f
			    (##sys#extend-macro-environment
			     name
			     (##sys#current-environment)
			     (##sys#eval/meta body))
			    (compile '(##core#undefined) e #f tf cntr se) ) )

			 ((##core#define-compiler-syntax)
			  (compile '(##core#undefined) e #f tf cntr se))

			 ((##core#let-compiler-syntax)
			  (compile 
			   (##sys#canonicalize-body (cddr x) se #f)
			   e #f tf cntr se))

			 ((##core#include)
			  (compile
			   `(##core#begin
			     ,@(##sys#include-forms-from-file (cadr x)))
			   e #f tf cntr se))

			 ((##core#let-module-alias)
			  (##sys#with-module-aliases
			   (map (lambda (b)
				  (##sys#check-syntax 'functor b '(symbol symbol))
				  (##sys#strip-syntax b))
				(cadr x))
			   (lambda ()
			     (compile `(##core#begin ,@(cddr x)) e #f tf cntr se))))

			 ((##core#module)
			  (let* ((x (##sys#strip-syntax x))
				 (name (cadr x))
				 (exports 
				  (or (eq? #t (caddr x))
				      (map (lambda (exp)
					     (cond ((symbol? exp) exp)
						   ((and (pair? exp) 
							 (let loop ((exp exp))
							   (or (null? exp)
							       (and (symbol? (car exp))
								    (loop (cdr exp))))))
						    exp)
						   (else
						    (##sys#syntax-error-hook
						     'module
						     "invalid export syntax" exp name))))
					   (caddr x)))))
			    (when (##sys#current-module)
			      (##sys#syntax-error-hook 'module "modules may not be nested" name))
			    (parameterize ((##sys#current-module 
					    (##sys#register-module name exports))
					   (##sys#current-environment '())
					   (##sys#macro-environment 
					    ##sys#initial-macro-environment)
					   (##sys#module-alias-environment
					    (##sys#module-alias-environment)))
			      (##sys#with-property-restore
			       (lambda ()
				 (let loop ((body (cdddr x)) (xs '()))
				   (if (null? body)
				       (let ((xs (reverse xs)))
					 (##sys#finalize-module (##sys#current-module))
					 (lambda (v)
					   (let loop2 ((xs xs))
					     (if (null? xs)
						 (##sys#void)
						 (let ((n (cdr xs)))
						   (cond ((pair? n)
							  ((car xs) v)
							  (loop2 n))
							 (else
							  ((car xs) v))))))))
				       (loop 
					(cdr body)
					(cons (compile 
					       (car body) 
					       '() #f tf cntr 
					       (##sys#current-environment))
					      xs))))) ) )))

			 [(##core#loop-lambda)
			  (compile `(,(rename 'lambda se) ,@(cdr x)) e #f tf cntr se) ]

			 [(##core#require-for-syntax)
			  (let ([ids (map (lambda (x) (##sys#eval/meta x))
					  (cdr x))])
			    (apply ##sys#require ids)
			    (let ([rs (##sys#lookup-runtime-requirements ids)])
			      (compile
			       (if (null? rs)
				   '(##core#undefined)
				   `(##sys#require ,@(map (lambda (x) `',x) rs)) )
			       e #f tf cntr se) ) ) ]

			 [(##core#require-extension)
			  (let ((imp? (caddr x)))
			    (compile
			     (let loop ([ids (##sys#strip-syntax (cadr x))])
			       (if (null? ids)
				   '(##core#undefined)
				   (let-values (((exp f real-id) 
						 (##sys#do-the-right-thing (car ids) #f imp?)))
				     `(##core#begin ,exp ,(loop (cdr ids))) ) ) )
			     e #f tf cntr se) ) ]

			 [(##core#elaborationtimeonly ##core#elaborationtimetoo) ; <- Note this!
			  (##sys#eval/meta (cadr x))
			  (compile '(##core#undefined) e #f tf cntr se) ]

			 [(##core#compiletimetoo)
			  (compile (cadr x) e #f tf cntr se) ]

			 [(##core#compiletimeonly ##core#callunit) 
			  (compile '(##core#undefined) e #f tf cntr se) ]

			 [(##core#declare)
			  (if (memq #:compiling ##sys#features)
			      (for-each (lambda (d) (##compiler#process-declaration d se)) (cdr x)) 
			      (##sys#notice
			       "declarations are ignored in interpreted code"
			       x) )
			  (compile '(##core#undefined) e #f tf cntr se) ]

			 [(##core#define-inline ##core#define-constant)
			  (compile `(,(rename 'define se) ,@(cdr x)) e #f tf cntr se) ]
                   
			 [(##core#primitive ##core#inline ##core#inline_allocate ##core#foreign-lambda 
					    ##core#define-foreign-variable 
					    ##core#define-external-variable ##core#let-location
					    ##core#foreign-primitive ##core#location
					    ##core#foreign-lambda* ##core#define-foreign-type)
			  (##sys#syntax-error-hook "cannot evaluate compiler-special-form" x) ]

			 [(##core#app)
			  (compile-call (cdr x) e tf cntr se) ]

			 ((##core#the)
			  (compile (cadddr x) e h tf cntr se))
			 
			 ((##core#typecase)
			  ;; drops exp and requires "else" clause
			  (cond ((assq 'else (##sys#strip-syntax (cdddr x))) =>
				 (lambda (cl)
				   (compile (cadr cl) e h tf cntr se)))
				(else
				 (##sys#syntax-error-hook
				  'compiler-typecase
				  "no `else-clause' in unresolved `compiler-typecase' form"
				  x))))

			 (else
			  (fluid-let ((##sys#syntax-context (cons head ##sys#syntax-context)))
			    (compile-call x e tf cntr se)))))))]
	      
	      [else
	       (emit-syntax-trace-info tf x cntr)
	       (compile-call x e tf cntr se)] ) )

      (define (fudge-argument-list n alst)
	(if (null? alst) 
	    (list alst)
	    (do ((n n (fx- n 1))
		 (c 0 (fx+ c 1))
		 (args alst 
		       (if (eq? '() args)
			   (##sys#error "bad argument count" n c)
			   (##sys#slot args 1)))
		 (last #f args) )
		((fx= n 0)
		 (##sys#setslot last 1 (list args))
		 alst) ) ) )

      (define (checked-length lst)
	(let loop ([lst lst] [n 0])
	  (cond [(null? lst) n]
		[(pair? lst) (loop (##sys#slot lst 1) (fx+ n 1))]
		[else #f] ) ) )

      (define (compile-call x e tf cntr se)
	(let* ((head (##sys#slot x 0))
	       (fn (if (procedure? head) 
		       (lambda _ head)
		       (compile (##sys#slot x 0) e #f tf cntr se)))
	       (args (##sys#slot x 1))
	       (argc (checked-length args))
	       (info x) )
	  (case argc
	    [(#f) (##sys#syntax-error/context "malformed expression" x)]
	    [(0) (lambda (v)
		   (emit-trace-info tf info cntr e v)
		   ((##core#app fn v)))]
	    [(1) (let ([a1 (compile (##sys#slot args 0) e #f tf cntr se)])
		   (lambda (v)
		     (emit-trace-info tf info cntr e v)
		     ((##core#app fn v) (##core#app a1 v))) ) ]
	    [(2) (let* ([a1 (compile (##sys#slot args 0) e #f tf cntr se)]
			[a2 (compile (##core#inline "C_u_i_list_ref" args 1) e #f tf cntr se)] )
		   (lambda (v)
		     (emit-trace-info tf info cntr e v)
		     ((##core#app fn v) (##core#app a1 v) (##core#app a2 v))) ) ]
	    [(3) (let* ([a1 (compile (##sys#slot args 0) e #f tf cntr se)]
			[a2 (compile (##core#inline "C_u_i_list_ref" args 1) e #f tf cntr se)]
			[a3 (compile (##core#inline "C_u_i_list_ref" args 2) e #f tf cntr se)] )
		   (lambda (v)
		     (emit-trace-info tf info cntr e v)
		     ((##core#app fn v) (##core#app a1 v) (##core#app a2 v) (##core#app a3 v))) ) ]
	    [(4) (let* ([a1 (compile (##sys#slot args 0) e #f tf cntr se)]
			[a2 (compile (##core#inline "C_u_i_list_ref" args 1) e #f tf cntr se)]
			[a3 (compile (##core#inline "C_u_i_list_ref" args 2) e #f tf cntr se)] 
			[a4 (compile (##core#inline "C_u_i_list_ref" args 3) e #f tf cntr se)] )
		   (lambda (v)
		     (emit-trace-info tf info cntr e v)
		     ((##core#app fn v) (##core#app a1 v) (##core#app a2 v) (##core#app a3 v) (##core#app a4 v))) ) ]
	    [else (let ([as (##sys#map (lambda (a) (compile a e #f tf cntr se)) args)])
		    (lambda (v)
		      (emit-trace-info tf info cntr e v)
		      (apply (##core#app fn v) (##sys#map (lambda (a) (##core#app a v)) as))) ) ] ) ) )

      (compile exp env #f (fx> (##sys#eval-debug-level) 0) cntr se) ) ) )


;;; evaluate in the macro-expansion/compile-time environment
(define (##sys#eval/meta form)
  (let ((oldcm (##sys#current-module))
	(oldme (##sys#macro-environment))
	(mme (##sys#meta-macro-environment))
	(aee (##sys#active-eval-environment)))
    (dynamic-wind
	(lambda () 
	  (##sys#current-module #f)
	  (##sys#macro-environment mme)
	  (##sys#active-eval-environment ##sys#current-meta-environment))
	(lambda ()
	  ((##sys#compile-to-closure
	    form
	    '() 
	    (##sys#current-meta-environment)) ;XXX evalenv? static?
	   '() ) )
	(lambda ()
	  (##sys#active-eval-environment aee)
	  (##sys#current-module oldcm)
	  (##sys#meta-macro-environment (##sys#macro-environment))
	  (##sys#macro-environment oldme)))))

(define ##sys#eval-handler 
  (make-parameter
   (lambda (x #!optional env)
     (let ((se (##sys#current-environment)))
       (cond (env
	      (##sys#check-structure env 'environment 'eval)
	      (let ((se2 (##sys#slot env 2)))
		((if se2		; not interaction-environment?
		     (parameterize ((##sys#macro-environment '()))
		       (##sys#compile-to-closure x '() se2 #f env (##sys#slot env 3)))
		     (##sys#compile-to-closure x '() se #f env #f))
		 '() ) ) )
	     (else
	      ((##sys#compile-to-closure x '() se #f #f #f) '() ) ) ) ) )))

(define eval-handler ##sys#eval-handler)

(define (eval x . env)
  (apply (##sys#eval-handler) 
	 x
	 env) )


;;; Setting properties dynamically scoped

(define-values (##sys#put/restore! ##sys#with-property-restore)
  (let ((trail '())
	(restoring #f))
    (values
     (lambda (sym prop val)
       (when restoring
	 (set! trail (cons (list sym prop (##sys#get sym prop)) trail)))
       (##sys#put! sym prop val)
       val)
     (lambda (thunk)
       (let ((t0 #f)
	     (r0 restoring))
	 (dynamic-wind
	     (lambda ()
	       (set! t0 trail)
	       (set! restoring #t))
	     thunk
	     (lambda ()
	       (do () ((eq? t0 trail))
		 (apply ##sys#put! (car trail))
		 (set! trail (cdr trail)))
	       (set! restoring r0))))))))


;;; Split lambda-list into its parts:

(define ##sys#decompose-lambda-list
  (let ([reverse reverse])
    (lambda (llist0 k)

      (define (err)
	(set! ##sys#syntax-error-culprit #f)
	(##sys#syntax-error-hook "illegal lambda-list syntax" llist0) )

      (let loop ([llist llist0] [vars '()] [argc 0])
	(cond [(eq? llist '()) (k (reverse vars) argc #f)]
	      [(not (##core#inline "C_blockp" llist)) (err)]
	      [(##core#inline "C_symbolp" llist) (k (reverse (cons llist vars)) argc llist)]
	      [(not (##core#inline "C_pairp" llist)) (err)]
	      [else (loop (##sys#slot llist 1)
			  (cons (##sys#slot llist 0) vars)
			  (fx+ argc 1) ) ] ) ) ) ) )


;;; Loading source/object files:

(define load-verbose (make-parameter (##sys#fudge 13)))

(define (##sys#abort-load) #f)
(define ##sys#current-source-filename #f)
(define ##sys#current-load-path "")
(define ##sys#dload-disabled #f)

(define-foreign-variable _dlerror c-string "C_dlerror")

(define (set-dynamic-load-mode! mode)
  (let ([mode (if (pair? mode) mode (list mode))]
	[now #f]
	[global #t] )
    (let loop ([mode mode])
      (when (pair? mode)
	(case (##sys#slot mode 0)
	  [(global) (set! global #t)]
	  [(local) (set! global #f)]
	  [(lazy) (set! now #f)]
	  [(now) (set! now #t)]
	  [else (##sys#signal-hook 'set-dynamic-load-mode! "invalid dynamic-load mode" (##sys#slot mode 0))] )
	(loop (##sys#slot mode 1)) ) )
    (##sys#set-dlopen-flags! now global) ) )

(let ([read read]
      [write write]
      [display display]
      [newline newline]
      [eval eval]
      [open-input-file open-input-file]
      [close-input-port close-input-port]
      [string-append string-append] 
      [topentry (##sys#make-c-string "C_toplevel")] )
  (define (has-sep? str)
    (let loop ([i (fx- (##sys#size str) 1)])
      (and (not (zero? i))
	   (if (memq (##core#inline "C_subchar" str i) '(#\\ #\/))
	       i
	       (loop (fx- i 1)) ) ) ) )
  (define (badfile x)
    (##sys#signal-hook #:type-error 'load "bad argument type - not a port or string" x) )
  (set! ##sys#load 
    (lambda (input evaluator pf #!optional timer printer)
      (when (string? input) 
	(set! input (##sys#expand-home-path input)) )
      (let* ((fname 
	      (cond [(port? input) #f]
		    [(not (string? input)) (badfile input)]
		    ((##sys#file-exists? input #t #f 'load) input)
		    (else
		     (let ([fname2 (##sys#string-append input ##sys#load-dynamic-extension)])
		       (if (and (not ##sys#dload-disabled)
				(##sys#fudge 24) ; dload?
				(##sys#file-exists? fname2 #t #f 'load))
			   fname2
			   (let ([fname3 (##sys#string-append input source-file-extension)])
			     (if (##sys#file-exists? fname3 #t #f 'load)
				 fname3
				 input) ) ) ) )))
	     [evproc (or evaluator eval)] )
	(cond [(and (string? input) (not fname))
	       (##sys#signal-hook #:file-error 'load "cannot open file" input) ]
	      [(and (load-verbose) fname)
	       (display "; loading ")
	       (display fname)
	       (display " ...\n") 
	       (flush-output)] )
	(or (and fname
		 (or (##sys#dload (##sys#make-c-string fname 'load) topentry) 
		     (and (not (has-sep? fname))
			  (##sys#dload 
			   (##sys#make-c-string
			    (##sys#string-append "./" fname) 
			    'load) 
			   topentry) ) ) )
	    (call-with-current-continuation
	     (lambda (abrt)
	       (fluid-let ((##sys#read-error-with-line-number #t)
			   (##sys#current-source-filename fname)
			   (##sys#current-load-path
			    (and fname
				 (let ((i (has-sep? fname)))
				   (if i (##sys#substring fname 0 (fx+ i 1)) "") ) ) )
			   (##sys#abort-load (lambda () (abrt #f))) )
		 (let ((in (if fname (open-input-file fname) input)))
		   (##sys#dynamic-wind
		    (lambda () #f)
		    (lambda ()
		      (let ((c1 (peek-char in)))
			(when (eq? c1 (integer->char 127))
			  (##sys#error 
			   'load 
			   (##sys#string-append 
			    "unable to load compiled module - " 
			    (or _dlerror "unknown reason"))
			   fname)))
		      (let ((x1 (read in)))
			(do ((x x1 (read in)))
			    ((eof-object? x))
			  (when printer (printer x))
			  (##sys#call-with-values
			   (lambda () 
			     (if timer
				 (time (evproc x)) 
				 (evproc x) ) )
			   (lambda results
			     (when pf
			       (for-each
				(lambda (r) 
				  (write r)
				  (newline) )
				results) ) ) ) ) ) )
		    (lambda () (close-input-port in)) ) ) ) ) ) )
	(##core#undefined) ) ) )
  (set! load
    (lambda (filename . evaluator)
      (##sys#load filename (optional evaluator #f) #f) ) )
  (set! load-relative
    (lambda (filename . evaluator)
      (##sys#load
       (if (memq (string-ref filename 0) '(#\\ #\/))
	   filename
	   (##sys#string-append ##sys#current-load-path filename) )
       (optional evaluator #f) #f) ) )
  (set! load-noisily
    (lambda (filename #!key (evaluator #f) (time #f) (printer #f))
      (##sys#load filename evaluator #t time printer) ) ) )

(define ##sys#load-library-extension 	; this is crude...
  (cond [(eq? (software-type) 'windows) windows-load-library-extension]
	[(eq? (software-version) 'macosx) macosx-load-library-extension]
	[(and (eq? (software-version) 'hpux) 
	      (eq? (machine-type) 'hppa)) hppa-load-library-extension]
	[else default-load-library-extension] ) )

(define ##sys#load-dynamic-extension default-load-library-extension)

(define ##sys#default-dynamic-load-libraries 
  (case (build-platform)
    ((cygwin) cygwin-default-dynamic-load-libraries)
    (else default-dynamic-load-libraries) ) )

(define dynamic-load-libraries 
  (let ((ext
	 (if (and (memq (software-version) '(linux netbsd openbsd freebsd))
		  (not (zero? binary-version))) ; allow "configless" build
	     (string-append
	      ##sys#load-library-extension
	      "." 
	      (number->string binary-version))
	     ##sys#load-library-extension)))
    (define complete
      (cut ##sys#string-append <> ext))
    (make-parameter
     (map complete ##sys#default-dynamic-load-libraries)
     (lambda (x)
       (##sys#check-list x)
       x) ) ) )

(define ##sys#load-library-0
  (let ([string-append string-append]
	[display display] )
    (lambda (uname lib)
      (let ([id (##sys#->feature-id uname)])
	(or (memq id ##sys#features)
	    (let ([libs
		   (if lib
		       (##sys#list lib)
		       (cons (##sys#string-append (##sys#slot uname 1) ##sys#load-library-extension)
			     (dynamic-load-libraries) ) ) ]
		  [top 
		   (##sys#make-c-string
		    (string-append 
		     "C_"
		     (##sys#string->c-identifier (##sys#slot uname 1)) 
		     "_toplevel") 'load-library) ] )
	      (when (load-verbose)
		(display "; loading library ")
		(display uname)
		(display " ...\n") )
	      (let loop ([libs libs])
		(cond [(null? libs) #f]
		      [(##sys#dload (##sys#make-c-string (##sys#slot libs 0) 'load-library) top)
		       (unless (memq id ##sys#features)
			 (set! ##sys#features (cons id ##sys#features)))
		       #t]
		      [else (loop (##sys#slot libs 1))] ) ) ) ) ) ) ) )

(define ##sys#load-library
  (lambda (uname . lib)
    (##sys#check-symbol uname 'load-library)
    (or (##sys#load-library-0 uname (and (pair? lib) (car lib)))
	(##sys#error 'load-library "unable to load library" uname _dlerror) ) ) )

(define load-library ##sys#load-library)

(define ##sys#include-forms-from-file
  (let ((with-input-from-file with-input-from-file)
	(read read)
	(reverse reverse))
    (lambda (fname)
      (let ((path (##sys#resolve-include-filename fname #t)))
	(when (load-verbose) (print "; including " path " ..."))
	(with-input-from-file path
	  (lambda ()
	    (fluid-let ((##sys#current-source-filename path))
	      (do ((x (read) (read))
		   (xs '() (cons x xs)) )
		  ((eof-object? x) 
		   (reverse xs))) ) ) ) ) ) ) )


;;; Extensions:

(define ##sys#canonicalize-extension-path
  (let ([string-append string-append])
    (lambda (id loc)
      (define (err) (##sys#error loc "invalid extension path" id))
      (define (sep? c) (or (char=? #\\ c) (char=? #\/ c)))
      (let ([p (cond [(string? id) id]
		     [(symbol? id) (##sys#symbol->string id)]
		     [(list? id) 
		      (let loop ([id id])
			(if (null? id)
			    ""
			    (string-append 
			     (let ([id0 (##sys#slot id 0)])
			       (cond [(symbol? id0) (##sys#symbol->string id0)]
				     [(string? id0) id0]
				     [else (err)] ) )
			     (if (null? (##sys#slot id 1))
				 ""
				 "/")
			     (loop (##sys#slot id 1)) ) ) ) ] ) ] )
	(let check ([p p])
	  (let ([n (##sys#size p)])
	    (cond [(fx= 0 n) (err)]
		  [(sep? (string-ref p 0))
		   (check (##sys#substring p 1 n)) ]
		  [(sep? (string-ref p (fx- n 1)))
		   (check (##sys#substring p 0 (fx- n 1))) ]
		  [else p] ) ) ) ) ) ) )

(define ##sys#repository-path
  (let ((rpath
	 (if (##sys#fudge 22)		; private repository?
	     (foreign-value "C_private_repository_path()" c-string)
	     (or (get-environment-variable repository-environment-variable)
		 (##sys#chicken-prefix 
		  (##sys#string-append 
		   "lib/chicken/"
		   (##sys#number->string (##sys#fudge 42))) )
		 install-egg-home))))
    (lambda (#!optional val)
      (if val
	  (set! rpath val)
	  rpath))))

(define repository-path ##sys#repository-path)

(define ##sys#setup-mode #f)

(define ##sys#find-extension
  (let ((string-append string-append) )
    (lambda (p inc?)
      (let ((rp (##sys#repository-path)))
	(define (check path)
	  (let ((p0 (string-append path "/" p)))
	    (and (or (and rp
			  (not ##sys#dload-disabled)
			  (##sys#fudge 24) ; dload?
			  (file-exists? (##sys#string-append p0 ##sys#load-dynamic-extension)))
		     (file-exists? (##sys#string-append p0 source-file-extension)) )
		 p0) ) )
	(let loop ((paths (##sys#append
			   (if ##sys#setup-mode '(".") '())
			   (if rp (list rp) '())
			   (if inc? ##sys#include-pathnames '())
			   (if ##sys#setup-mode '() '("."))) ))
	  (and (pair? paths)
	       (let ((pa (##sys#slot paths 0)))
		 (or (check pa)
		     (loop (##sys#slot paths 1)) ) ) ) ) ) ) ))

(define ##sys#loaded-extensions '())

(define ##sys#load-extension
  (let ((string->symbol string->symbol))
    (lambda (id loc #!optional (err? #t))
      (cond ((string? id) (set! id (string->symbol id)))
	    (else (##sys#check-symbol id loc)) )
      (let ([p (##sys#canonicalize-extension-path id loc)])
	(cond ((member p ##sys#loaded-extensions))
	      ((or (memq id ##sys#core-library-modules)
		   (memq id ##sys#core-syntax-modules))
	       (or (##sys#load-library-0 id #f)
		   (and err?
			(##sys#error loc "cannot load core library" id))))
	      (else
	       (let ([id2 (##sys#find-extension p #f)])
		 (cond (id2
			(##sys#load id2 #f #f)
			(set! ##sys#loaded-extensions (cons p ##sys#loaded-extensions)) 
			#t)
		       (err? (##sys#error loc "cannot load extension" id))
		       (else #f) ) ) ) ) ) ) ) )

(define (##sys#provide . ids)
  (for-each
   (lambda (id)
     (##sys#check-symbol id 'provide)
     (let ([p (##sys#canonicalize-extension-path id 'provide)])
       (set! ##sys#loaded-extensions (cons p ##sys#loaded-extensions)) ) ) 
   ids) )

(define provide ##sys#provide)

(define (##sys#provided? id)
  (and (member (##sys#canonicalize-extension-path id 'provided?) ##sys#loaded-extensions) 
       #t) )

(define provided? ##sys#provided?)

(define ##sys#require
  (lambda ids
    (for-each
     (cut ##sys#load-extension <> 'require) 
     ids) ) )

(define require ##sys#require)

(define ##sys#extension-information
  (let ([with-input-from-file with-input-from-file]
	[string-append string-append]
	[read read] )
    (lambda (id loc)
      (and-let* ((rp (##sys#repository-path)))
	(let* ((p (##sys#canonicalize-extension-path id loc))
	       (rpath (string-append rp "/" p ".")) )
	  (cond ((file-exists? (string-append rpath setup-file-extension))
		 => (cut with-input-from-file <> read) )
		(else #f) ) ) ) ) ))

(define (extension-information ext)
  (##sys#extension-information ext 'extension-information) )

(define ##sys#lookup-runtime-requirements 
  (let ([with-input-from-file with-input-from-file]
	[read read] )
    (lambda (ids)
      (let loop1 ([ids ids])
	(if (null? ids)
	    '()
	    (append
	     (or (and-let* ([info (##sys#extension-information (car ids) #f)]
			    [a (assq 'require-at-runtime info)] )
		   (cdr a) )
		 '() )
	     (loop1 (cdr ids)) ) ) ) ) ) )

(define ##sys#do-the-right-thing
  (let ((vector->list vector->list))
    (lambda (id comp? imp?)
      (define (add-req id syntax?)
	(when comp?
	  (##sys#hash-table-update!
	   ##compiler#file-requirements
	   (if syntax? 'dynamic/syntax 'dynamic)
	   (cut lset-adjoin eq? <> id) ;XXX assumes compiler has srfi-1 loaded
	   (lambda () (list id)))))
      (define (impform x id builtin?)
	`(##core#begin
	  ,x
	  ,@(if (and imp? (or (not builtin?) (##sys#current-module)))
		`((import ,id))		;XXX make hygienic
		'())))
      (define (srfi-id n)
	(if (fixnum? n)
	    (##sys#intern-symbol
	     (##sys#string-append "srfi-" (##sys#number->string n)))
	    (##sys#syntax-error 'require-extension "invalid SRFI number" n)))
      (define (doit id impid)
	(cond ((or (memq id builtin-features)
		   (if comp?
		       (memq id builtin-features/compiled)
		       (##sys#feature? id) ) )
	       (values (impform '(##core#undefined) impid #t) #t id) )
	      ((memq id ##sys#core-library-modules)
	       (values
		(impform
		 (if comp?
		     `(##core#declare (uses ,id))
		     `(##sys#load-library ',id #f) )
		 impid #f)
		#t id) )
	      ((memq id ##sys#core-syntax-modules)
	       (values
		(impform
		 (if comp?
		     `(##core#declare (uses ,id))
		     `(##sys#load-library ',id #f) )
		 impid #t)
		#t id) )
	      ((memq id ##sys#explicit-library-modules)
	       (let* ((info (##sys#extension-information id 'require-extension))
		      (nr (assq 'import-only info))
		      (s (assq 'syntax info)))
		 (values
		  `(##core#begin
		    ,@(if s `((##core#require-for-syntax ',id)) '())
		    ,(impform
		      (if (not nr)
			  (if comp?
			      `(##core#declare (uses ,id)) 
			      `(##sys#load-library ',id #f) )
			  '(##core#undefined))
		      impid #f))
		  #t id) ) )
	      (else
	       (let ((info (##sys#extension-information id 'require-extension)))
		 (cond (info
			(let ((s (assq 'syntax info))
			      (nr (assq 'import-only info))
			      (rr (assq 'require-at-runtime info)) )
			  (when s (add-req id #t))
			  (values 
			   (impform
			    `(##core#begin
			      ,@(if s `((##core#require-for-syntax ',id)) '())
			      ,@(if (or nr (and (not rr) s))
				    '()
				    (begin
				      (add-req id #f)
				      `((##sys#require
					 ,@(map (lambda (id) `',id)
						(cond (rr (cdr rr))
						      (else (list id)) ) ) ) ) ) ) )
			    impid #f)
			   #t id) ) )
		       (else
			(add-req id #f)
			(values
			 (impform
			  `(##sys#require ',id) 
			  impid #f)
			 #f id)))))))
      (cond ((and (pair? id) (symbol? (car id)))
	     (case (car id)
	       ((srfi)
		(let* ((f #f)
		       (exp
			`(##core#begin
			  ,@(map (lambda (n)
				   (let ((rid (srfi-id n)))
				     (let-values (((exp f2 _) (doit rid rid)))
				       (set! f (or f f2))
				       exp)))
				 (cdr id)))))
		  (values exp f id)))	;XXX `id' not fully correct
	       ((rename except only prefix)
		(let follow ((id2 id))
		  (if (and (pair? id2) (pair? (cdr id2)))
		      (if (and (eq? 'srfi (car id2)) (null? (cddr id2))) ; only allow one number
			  (doit (srfi-id (cadr id2)) id)
			  (follow (cadr id2)))
		      (doit id2 id))))
	       (else (##sys#error "invalid extension specifier" id) ) ) )
	    ((symbol? id)
	     (doit id id))
	    (else (##sys#error "invalid extension specifier" id) ) ) )))


;;; Convert string into valid C-identifier:

(define (##sys#string->c-identifier str)
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


;;; Environments:

(define interaction-environment
  (let ((e (##sys#make-structure 'environment 'interaction-environment #f #f)))
    (lambda () e)))

(define-record-printer (environment e p)
  (##sys#print "#<environment " #f p)
  (##sys#print (##sys#slot e 1) #f p)
  (##sys#write-char-0 #\> p))

(define scheme-report-environment)
(define null-environment)

(let* ((r4s (module-environment 'r4rs 'scheme-report-environment/4))
       (r5s (module-environment 'scheme 'scheme-report-environment/5))
       (r4n (module-environment 'r4rs-null 'null-environment/4))
       (r5n (module-environment 'r5rs-null 'null-environment/5)))
  (define (strip se)
    (foldr
     (lambda (s r)
       (if (memq (car s)
		 '(import 
		    require-extension 
		    require-library 
		    begin-for-syntax
		    export 
		    module
		    cond-expand
		    syntax
		    reexport 
		    import-for-syntax))
	   r
	   (cons s r)))
     '()
     se))
  ;; Strip non-std syntax from SEs
  (##sys#setslot r4s 2 (strip (##sys#slot r4s 2)))
  (##sys#setslot r4n 2 (strip (##sys#slot r4n 2)))
  (##sys#setslot r5s 2 (strip (##sys#slot r5s 2)))
  (##sys#setslot r5n 2 (strip (##sys#slot r5n 2)))
  (set! scheme-report-environment
    (lambda (n)
      (##sys#check-exact n 'scheme-report-environment)
      (case n
	((4) r4s)
	((5) r5s)
	(else 
	 (##sys#error 
	  'scheme-report-environment
	  "unsupported scheme report environment version" n)) ) ) )
  (set! null-environment
    (lambda (n)
      (##sys#check-exact n 'null-environment)
      (case n
	((4) r4n)
	((5) r5n)
	(else
	 (##sys#error
	  'null-environment 
	  "unsupported null environment version" n) )))))
            

;;; Find included file:

(define ##sys#include-pathnames 
  (let ((h (chicken-home)))
    (if h (list h) '())) )

(define ##sys#resolve-include-filename
  (let ((string-append string-append) )
    (define (exists? fname)
      (##sys#file-exists? fname #t #f #f))
    (lambda (fname prefer-source #!optional repo)
      (define (test2 fname lst)
	(if (null? lst)
	    (and (exists? fname) fname)
	    (let ([fn (##sys#string-append fname (car lst))])
	      (if (exists? fn)
		  fn
		  (test2 fname (cdr lst)) ) ) ) )
      (define (test fname)
	(test2 
	 fname
	 (cond ((not (##sys#fudge 24)) (list source-file-extension)) ; no dload?
	       (prefer-source (list source-file-extension ##sys#load-dynamic-extension))
	       (else (list ##sys#load-dynamic-extension source-file-extension) ) ) ))
      (or (test fname)
	  (let loop ((paths (if repo
				(##sys#append 
				 ##sys#include-pathnames 
				 (let ((rp (##sys#repository-path)))
				   (if rp
				       (list (##sys#repository-path))
				       '())))
				##sys#include-pathnames) ) )
	    (cond ((eq? paths '()) fname)
		  ((test (string-append (##sys#slot paths 0)
					"/"
					fname) ) )
		  (else (loop (##sys#slot paths 1))) ) ) ) ) ) )


;;;; Read-Eval-Print loop:

(define ##sys#repl-print-length-limit #f)
(define ##sys#repl-read-hook #f)
(define ##sys#repl-recent-call-chain #f) ; used in csi for ,c command

(define (##sys#repl-print-hook x port)
  (##sys#with-print-length-limit ##sys#repl-print-length-limit (cut ##sys#print x #t port))
  (##sys#write-char-0 #\newline port) )

(define repl-prompt (make-parameter (lambda () "#;> ")))

(define ##sys#read-prompt-hook
  (let ([repl-prompt repl-prompt])
    (lambda () 
      (##sys#print ((repl-prompt)) #f ##sys#standard-output)
      (##sys#flush-output ##sys#standard-output) ) ) )

(define ##sys#clear-trace-buffer (foreign-lambda void "C_clear_trace_buffer"))

(define repl
  (let ((eval eval)
	(read read)
	(call-with-current-continuation call-with-current-continuation)
	(string-append string-append))
    (lambda (#!optional (evaluator eval))

      (define (write-err xs)
	(for-each (cut ##sys#repl-print-hook <> ##sys#standard-error) xs) )

      (define (write-results xs)
	(cond ((null? xs)
	       (##sys#print "; no values\n" #f ##sys#standard-output))
	      ((not (eq? (##core#undefined) (car xs)))
	       (for-each (cut ##sys#repl-print-hook <> ##sys#standard-output) xs)
	       (when (pair? (cdr xs))
		 (##sys#print 
		  (string-append "; " (##sys#number->string (length xs)) " values\n")
		  #f ##sys#standard-output)))))

      (let ((stdin ##sys#standard-input)
	    (stdout ##sys#standard-output)
	    (stderr ##sys#standard-error)
	    (ehandler (##sys#error-handler))
	    (rhandler (##sys#reset-handler)) 
	    (lv #f)
	    (qh ##sys#quit-hook)
	    (uie ##sys#unbound-in-eval) )

	(define (saveports)
	  (set! stdin ##sys#standard-input)
	  (set! stdout ##sys#standard-output)
	  (set! stderr ##sys#standard-error) )

	(define (resetports)
	  (set! ##sys#standard-input stdin)
	  (set! ##sys#standard-output stdout)
	  (set! ##sys#standard-error stderr) )

	(call-with-current-continuation
	 (lambda (k)
	   (##sys#dynamic-wind
	    (lambda ()
	      (set! lv (load-verbose))
	      (set! ##sys#quit-hook (lambda (result) (k result)))
	      (load-verbose #t)
	      (##sys#error-handler
	       (lambda (msg . args)
		 (resetports)
		 (##sys#print "\nError" #f ##sys#standard-error)
		 (when msg
		   (##sys#print ": " #f ##sys#standard-error)
		   (##sys#print msg #f ##sys#standard-error) )
		 (if (and (pair? args) (null? (cdr args)))
		     (begin
		       (##sys#print ": " #f ##sys#standard-error)
		       (write-err args) )
		     (begin
		       (##sys#write-char-0 #\newline ##sys#standard-error)
		       (write-err args) ) )
		 (set! ##sys#repl-recent-call-chain
		   (or (and-let* ((lexn ##sys#last-exception) ;XXX not really right
				  ((##sys#structure? lexn 'condition))
				  (a (member '(exn . call-chain) (##sys#slot lexn 2))))
			 (let ((ct (cadr a)))
			   (##sys#really-print-call-chain
			    ##sys#standard-error ct
			    "\n\tCall history:\n")
			   ct))
		       (print-call-chain ##sys#standard-error)))
		 (flush-output ##sys#standard-error) ) ) )
	    (lambda ()
	      (let loop ()
		(saveports)
		(call-with-current-continuation
		 (lambda (c)
		   (##sys#reset-handler
		    (lambda ()
		      (set! ##sys#read-error-with-line-number #f)
		      (set! ##sys#enable-qualifiers #t)
		      (resetports)
		      (c #f) ) ) ) )
		(##sys#read-prompt-hook)
		(let ([exp ((or ##sys#repl-read-hook read))])
		  (unless (eof-object? exp)
		    (when (eq? #\newline (##sys#peek-char-0 ##sys#standard-input))
		      (##sys#read-char-0 ##sys#standard-input) )
		    (##sys#clear-trace-buffer)
		    (set! ##sys#unbound-in-eval '())
		    (receive result (evaluator exp)
		      (when (and ##sys#warnings-enabled (pair? ##sys#unbound-in-eval))
			(let loop ((vars ##sys#unbound-in-eval) (u '()))
			  (cond ((null? vars)
				 (when (pair? u)
				   (##sys#notice
				    "the following toplevel variables are referenced but unbound:\n")
				   (for-each 
				    (lambda (v)
				      (##sys#print "  " #f ##sys#standard-error)
				      (##sys#print (car v) #t ##sys#standard-error)
				      (when (cdr v)
					(##sys#print " (in " #f ##sys#standard-error)
					(##sys#print (cdr v) #t ##sys#standard-error) 
					(##sys#write-char-0 #\) ##sys#standard-error) )
				      (##sys#write-char-0 #\newline ##sys#standard-error) )
				    u)
				   (##sys#flush-output ##sys#standard-error)))
				((or (memq (caar vars) u) 
				     (##sys#symbol-has-toplevel-binding? (caar vars)) )
				 (loop (cdr vars) u) )
				(else (loop (cdr vars) (cons (car vars) u))) ) 9 ) )
		      (write-results result) 
		      (loop) ) ) ) ) )
	    (lambda ()
	      (load-verbose lv)
	      (set! ##sys#quit-hook qh)
	      (set! ##sys#unbound-in-eval uie)
	      (##sys#error-handler ehandler)
	      (##sys#reset-handler rhandler) ) ) ) ) ) ) ))


;;; SRFI-10:

(define ##sys#sharp-comma-reader-ctors (make-vector 301 '()))

(define (define-reader-ctor spec proc)
  (##sys#check-symbol spec 'define-reader-ctor)
  (##sys#hash-table-set! ##sys#sharp-comma-reader-ctors spec proc) )

(set! ##sys#user-read-hook
  (let ((old ##sys#user-read-hook)
	(read-char read-char)
	(read read) )
    (lambda (char port)
      (cond ((char=? char #\,)
	     (read-char port)
	     (let* ((exp (read port))
		    (err (lambda () (##sys#read-error port "invalid sharp-comma external form" exp))) )
	       (if (or (null? exp) (not (list? exp)))
		   (err)
		   (let ([spec (##sys#slot exp 0)])
		     (if (not (symbol? spec))
			 (err) 
			 (let ((ctor (##sys#hash-table-ref ##sys#sharp-comma-reader-ctors spec)))
			   (if ctor
			       (apply ctor (##sys#slot exp 1))
			       (##sys#read-error port "undefined sharp-comma constructor" spec) ) ) ) ) ) ) )
	    (else (old char port)) ) ) ) )


;;; Simple invocation API:

(declare
  (hide last-error run-safe store-result store-string
	CHICKEN_yield CHICKEN_apply_to_string
	CHICKEN_eval CHICKEN_eval_string CHICKEN_eval_to_string CHICKEN_eval_string_to_string
	CHICKEN_apply CHICKEN_eval_apply CHICKEN_eval_to_string
	CHICKEN_read CHICKEN_load CHICKEN_get_error_message) )
	
(define last-error #f)

(define (run-safe thunk)
  (set! last-error #f)
  (handle-exceptions ex 
      (let ((o (open-output-string)))
	(print-error-message ex o)
	(set! last-error (get-output-string o))
	#f)
    (thunk) ) )

#>
#define C_store_result(x, ptr)   (*((C_word *)C_block_item(ptr, 0)) = (x), C_SCHEME_TRUE)
<#

(define (store-result x result)
  (##sys#gc #f)
  (when result
    (##core#inline "C_store_result" x result) )
  #t)

(define-external (CHICKEN_yield) bool
  (run-safe (lambda () (begin (##sys#thread-yield!) #t))) )

(define-external (CHICKEN_eval (scheme-object exp) ((c-pointer "C_word") result)) bool
  (run-safe
   (lambda ()
     (store-result (eval exp) result) ) ) )

(define-external (CHICKEN_eval_string (c-string str) ((c-pointer "C_word") result)) bool
  (run-safe
   (lambda ()
     (let ([i (open-input-string str)])
       (store-result (eval (read i)) result)) )))

#>
#define C_copy_result_string(str, buf, n)  (C_memcpy((char *)C_block_item(buf, 0), C_c_string(str), C_unfix(n)), ((char *)C_block_item(buf, 0))[ C_unfix(n) ] = '\0', C_SCHEME_TRUE)
<#

(define (store-string str bufsize buf)
  (let ((len (##sys#size str)))
    (cond ((fx>= len bufsize)
	   (set! last-error "Error: not enough room for result string")
	   #f)
	  (else (##core#inline "C_copy_result_string" str buf len)) ) ) )

(define-external (CHICKEN_eval_to_string (scheme-object exp) ((c-pointer "char") buf)
					  (int bufsize))
  bool
  (run-safe
   (lambda ()
     (let ([o (open-output-string)])
       (write (eval exp) o) 
       (store-string (get-output-string o) bufsize buf)) ) ) )

(define-external (CHICKEN_eval_string_to_string (c-string str) ((c-pointer "char") buf)
						 (int bufsize) ) 
  bool
  (run-safe
   (lambda ()
     (let ([o (open-output-string)])
       (write (eval (read (open-input-string str))) o)
       (store-string (get-output-string o) bufsize buf)) ) ) )

(define-external (CHICKEN_apply (scheme-object func) (scheme-object args) 
				 ((c-pointer "C_word") result))
  bool
  (run-safe (lambda () (store-result (apply func args) result))) )

(define-external (CHICKEN_apply_to_string (scheme-object func) (scheme-object args) 
					   ((c-pointer "char") buf) (int bufsize))
  bool
  (run-safe
   (lambda ()
     (let ([o (open-output-string)])
       (write (apply func args) o) 
       (store-string (get-output-string o) bufsize buf)) ) ) )

(define-external (CHICKEN_read (c-string str) ((c-pointer "C_word") result)) bool
  (run-safe
   (lambda ()
     (let ([i (open-input-string str)])
       (store-result (read i) result) ) ) ) )

(define-external (CHICKEN_load (c-string str)) bool
  (run-safe (lambda () (load str) #t)) )

(define-external (CHICKEN_get_error_message ((c-pointer "char") buf) (int bufsize)) void
  (store-string (or last-error "No error") bufsize buf) )
