;;;; expand.scm - The HI/LO expander
;
; Copyright (c) 2008-2013, The Chicken Team
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


;; this unit needs the "modules" unit, but must be initialized first, so it doesn't
;; declare "modules" as used - if you use "-explicit-use", take care of this.

(declare
  (unit expand)
  (disable-interrupts)
  (fixnum)
  (hide match-expression
	macro-alias
	d dd dm dx map-se
	lookup check-for-redef) 
  (not inline ##sys#syntax-error-hook ##sys#compiler-syntax-hook
       ##sys#toplevel-definition-hook))

(include "common-declarations.scm")

(define-syntax d (syntax-rules () ((_ . _) (void))))

(set! ##sys#features
  (append '(#:hygienic-macros 
	    #:syntax-rules 
	    #:srfi-0 #:srfi-2 #:srfi-6 #:srfi-9 #:srfi-46 #:srfi-55 #:srfi-61) 
	  ##sys#features))

(define-alias dd d)
(define-alias dm d)
(define-alias dx d)

(define-inline (getp sym prop)
  (##core#inline "C_i_getprop" sym prop #f))

(define-inline (putp sym prop val)
  (##core#inline_allocate ("C_a_i_putprop" 8) sym prop val))


;;; Syntactic environments

(define ##sys#current-environment (make-parameter '()))
(define ##sys#current-meta-environment (make-parameter '()))

;;XXX should this be in eval.scm?
(define ##sys#active-eval-environment (make-parameter ##sys#current-environment))

(define (lookup id se)
  (cond ((##core#inline "C_u_i_assq" id se) => cdr)
	((getp id '##core#macro-alias))
	(else #f)))

(define (macro-alias var se)
  (if (or (##sys#qualified-symbol? var)
	  (let* ((str (##sys#slot var 1))
		 (len (##sys#size str)))
	    (and (fx> len 0)
		 (char=? #\# (##core#inline "C_subchar" str 0)))))
      var
      (let* ((alias (gensym var))
	     (ua (or (lookup var se) var))
             (rn (or (getp var '##core#real-name) var)))
	(putp alias '##core#macro-alias ua)
	(putp alias '##core#real-name rn)
	(dd "aliasing " alias " (real: " var ") to " 
	    (if (pair? ua)
		'<macro>
		ua))
	alias) ) )

#+debugbuild
(define (map-se se)
  (map (lambda (a) 
	 (cons (car a) (if (symbol? (cdr a)) (cdr a) '<macro>)))
       se))

(define (##sys#strip-syntax exp)
 ;; if se is given, retain bound vars
 (let ((seen '()))
   (let walk ((x exp))
     (cond ((assq x seen) => cdr)
           ((symbol? x)
            (let ((x2 (getp x '##core#macro-alias) ) )
              (cond ((getp x '##core#real-name))
                    ((not x2) x)
                    ((pair? x2) x)
                    (else x2))))
           ((pair? x)
            (let ((cell (cons #f #f)))
              (set! seen (cons (cons x cell) seen))
              (set-car! cell (walk (car x)))
              (set-cdr! cell (walk (cdr x)))
              cell))
           ((vector? x)
            (let* ((len (##sys#size x))
		   (vec (make-vector len)))
              (set! seen (cons (cons x vec) seen))
              (do ((i 0 (fx+ i 1)))
                  ((fx>= i len) vec)
                (##sys#setslot vec i (walk (##sys#slot x i))))))
           (else x)))))

(define strip-syntax ##sys#strip-syntax)

(define (##sys#extend-se se vars #!optional (aliases (map gensym vars)))
  (for-each
   (lambda (alias sym)
     (let ((original-real-name (getp sym '##core#real-name)))
       (putp alias '##core#real-name (or original-real-name sym))))
   aliases vars)
  (append (map (lambda (x y) (cons x y)) vars aliases) se)) ; inline cons


;;; resolve symbol to global name

(define (##sys#globalize sym se)
  (let loop1 ((sym sym))
    (cond ((not (symbol? sym)) sym)
	  ((getp sym '##core#macro-alias) =>
	   (lambda (a) (if (symbol? a) (loop1 a) sym)))
	  (else
	   (let loop ((se se))		; ignores syntax bindings
	     (cond ((null? se)
		    (##sys#alias-global-hook sym #t #f)) ;XXX could hint at decl (3rd arg)
		   ((and (eq? sym (caar se)) (symbol? (cdar se))) (cdar se))
		   (else (loop (cdr se)))))))))


;;; Macro handling

(define ##sys#macro-environment (make-parameter '()))
(define ##sys#chicken-macro-environment '()) ; used later in chicken.import.scm
(define ##sys#chicken-ffi-macro-environment '()) ; used later in foreign.import.scm

(define (##sys#ensure-transformer t #!optional loc)
  (cond ((procedure? t) (##sys#slot (##sys#er-transformer t) 1)) ; DEPRECATED
	((##sys#structure? t 'transformer) (##sys#slot t 1))
	(else (##sys#error loc "expected syntax-transformer, but got" t))))

(define (##sys#extend-macro-environment name se transformer)
  (let ((me (##sys#macro-environment))
	(handler (##sys#ensure-transformer transformer name)))
    (cond ((lookup name me) =>
	   (lambda (a)
	     (set-car! a se)
	     (set-car! (cdr a) handler)
	     a))
	  (else 
	   (let ((data (list se handler)))
	     (##sys#macro-environment
	      (cons (cons name data) me))
	     data)))))

(define (##sys#copy-macro old new)
  (let ((def (lookup old (##sys#macro-environment))))
    (apply ##sys#extend-macro-environment new def) ) )

(define (##sys#macro? sym #!optional (senv (##sys#current-environment)))
  (or (let ((l (lookup sym senv)))
	(pair? l))
      (and-let* ((l (lookup sym (##sys#macro-environment))))
	(pair? l))))

(define (##sys#unregister-macro name)
  (##sys#macro-environment
    ;; this builds up stack, but isn't used often anyway...
    (let loop ((me (##sys#macro-environment)) (me2 '()))
      (cond ((null? me) '())
	    ((eq? name (caar me)) (cdr me))
	    (else (cons (car me) (loop (cdr me))))))))

(define (##sys#undefine-macro! name)
  (##sys#unregister-macro name) )


;; The basic macro-expander

(define (##sys#expand-0 exp dse cs?)
  (define (call-handler name handler exp se cs)
    (dd "invoking macro: " name)
    (dd `(STATIC-SE: ,@(map-se se)))
    (handle-exceptions ex
	;; modify error message in condition object to include 
	;; currently expanded macro-name
	(##sys#abort
	 (if (and (##sys#structure? ex 'condition)
		  (memv 'exn (##sys#slot ex 1)) )
	     (##sys#make-structure
	      'condition
	      (##sys#slot ex 1)
	      (let copy ([ps (##sys#slot ex 2)])
		(if (null? ps)
		    '()
		    (let ([p (car ps)]
			  [r (cdr ps)])
		      (if (and (equal? '(exn . message) p)
			       (pair? r)
			       (string? (car r)) )
			  (cons 
			   '(exn . message)
			   (cons (string-append
				  "during expansion of ("
				  (##sys#slot name 1) 
				  " ...) - "
				  (car r) )
				 (cdr r) ) )
			  (copy r) ) ) ) ) )
	     ex) )
      (let ((exp2
	     (if cs
		 ;; compiler-syntax may "fall through"
		 (fluid-let ((##sys#syntax-rules-mismatch (lambda (input) exp))) ; a bit of a hack
		   (handler exp se dse))
		 (handler exp se dse))) )
	(when (and (not cs) (eq? exp exp2))
	  (##sys#syntax-error-hook
	   (string-append
	    "syntax transformer for `" (symbol->string name)
	    "' returns original form, which would result in endless expansion")
	   exp))
	(dx `(,name --> ,exp2))
	exp2)))
  (define (expand head exp mdef)
    (dd `(EXPAND: 
	  ,head 
	  ,(cond ((getp head '##core#macro-alias) =>
		  (lambda (a) (if (symbol? a) a '<macro>)) )
		 (else '_))
	  ,exp 
	  ,(if (pair? mdef)
	       `(SE: ,@(map-se (car mdef)))
	       mdef)))
    (cond ((not (list? exp))
	   (##sys#syntax-error-hook "invalid syntax in macro form" exp) )
	  ((pair? mdef)
	   (values 
	    ;; force ref. opaqueness by passing dynamic se [what does this comment mean? I forgot ...]
	    (call-handler head (cadr mdef) exp (car mdef) #f)
	    #t))
	  (else (values exp #f)) ) )
  (let loop ((exp exp))
    (if (pair? exp)
      (let ((head (car exp))
	    (body (cdr exp)) )
	(if (symbol? head)
	    (let ((head2 (or (lookup head dse) head)))
	      (unless (pair? head2)
		(set! head2 (or (lookup head2 (##sys#macro-environment)) head2)) )
	      (cond [(eq? head2 '##core#let)
		     (##sys#check-syntax 'let body '#(_ 2) #f dse)
		     (let ([bindings (car body)])
		       (cond [(symbol? bindings) ; expand named let
			      (##sys#check-syntax 'let body '(_ #((variable _) 0) . #(_ 1)) #f dse)
			      (let ([bs (cadr body)])
				(values
				 `(##core#app
				   (##core#letrec
				    ([,bindings 
				      (##core#loop-lambda
				       ,(map (lambda (b) (car b)) bs) ,@(cddr body))])
				    ,bindings)
				   ,@(##sys#map cadr bs) )
				 #t) ) ]
			     [else (values exp #f)] ) ) ]
		    ((and cs? (symbol? head2) (getp head2 '##compiler#compiler-syntax)) =>
		     (lambda (cs)
		       (let ((result (call-handler head (car cs) exp (cdr cs) #t)))
			 (cond ((eq? result exp) (expand head exp head2))
			       (else
				(when ##sys#compiler-syntax-hook
				  (##sys#compiler-syntax-hook head result))
				(loop result))))))
		    [else (expand head exp head2)] ) )
	    (values exp #f) ) )
      (values exp #f) ) ) )

(define ##sys#compiler-syntax-hook #f)
(define ##sys#enable-runtime-macros #f)



;;; User-level macroexpansion

(define (##sys#expand exp #!optional (se (##sys#current-environment)) cs?)
  (let loop ((exp exp))
    (let-values (((exp2 m) (##sys#expand-0 exp se cs?)))
      (if m
	  (loop exp2)
	  exp2) ) ) )

(define expand ##sys#expand)


;;; Extended (DSSSL-style) lambda lists
;
; Assumptions:
;
; 1) #!rest must come before #!key
; 2) default values may refer to earlier variables
; 3) optional/key args may be either variable or (variable default)
; 4) an argument marker may not be specified more than once
; 5) no special handling of extra keywords (no error)
; 6) default value of optional/key args is #f
; 7) mixing with dotted list syntax is allowed

(define (##sys#extended-lambda-list? llist)
  (let loop ([llist llist])
    (and (pair? llist)
	 (case (##sys#slot llist 0)
	   [(#!rest #!optional #!key) #t]
	   [else (loop (cdr llist))] ) ) ) )

(define ##sys#expand-extended-lambda-list
  (let ([reverse reverse])
    (lambda (llist0 body errh se)
      (define (err msg) (errh msg llist0))
      (define (->keyword s) (string->keyword (##sys#slot s 1)))
      (let ([rvar #f]
	    [hasrest #f] 
	    (%let* (macro-alias 'let* se))
	    (%lambda '##core#lambda)
	    (%opt (macro-alias 'optional se))
	    (%let-optionals* (macro-alias 'let-optionals* se))
	    (%let (macro-alias 'let se)))
	(let loop ([mode 0]		; req=0, opt=1, rest=2, key=3, end=4
		   [req '()]
		   [opt '()]
		   [key '()] 
		   [llist llist0] )
	  (cond [(null? llist)
		 (values 
		  (if rvar (##sys#append (reverse req) rvar) (reverse req))
		  (let ([body 
			 (if (null? key)
			     body
			     `((,%let*
				,(map (lambda (k)
					(let ([s (car k)])
					  `(,s (##sys#get-keyword
						(##core#quote ,(->keyword (##sys#strip-syntax s))) ,(or hasrest rvar)
						,@(if (pair? (cdr k)) 
						      `((,%lambda () ,@(cdr k)))
						      '())))))
				      (reverse key) )
				,@body) ) ) ] )
		    (cond [(null? opt) body]
			  [(and (not hasrest) (null? key) (null? (cdr opt)))
			   `((,%let
			      ([,(caar opt) (,%opt ,rvar ,(cadar opt))])
			      ,@body) ) ]
			  [(and (not hasrest) (null? key))
			   `((,%let-optionals*
			      ,rvar ,(reverse opt) ,@body))]
			  [else 
			   `((,%let-optionals*
			      ,rvar ,(##sys#append (reverse opt) (list (or hasrest rvar))) 
			      ,@body))] ) ) ) ]
		[(symbol? llist) 
		 (if (fx> mode 2)
		     (err "rest argument list specified more than once")
		     (begin
		       (unless rvar (set! rvar llist))
		       (set! hasrest llist)
		       (loop 4 req opt '() '()) ) ) ]
		[(not (pair? llist))
		 (err "invalid lambda list syntax") ]
		[else
		 (let* ((var (car llist))
			(x (or (and (symbol? var) (not (eq? 3 mode)) (lookup var se)) var))
			(r (cdr llist)))
		   (case x
		     [(#!optional)
		      (unless rvar (set! rvar (macro-alias 'tmp se)))
		      (if (eq? mode 0)
			  (loop 1 req '() '() r)
			  (err "`#!optional' argument marker in wrong context") ) ]
		     [(#!rest)
		      (if (fx<= mode 1)
			  (if (and (pair? r) (symbol? (car r)))
			      (begin
				(if (not rvar) (set! rvar (car r)))
				(set! hasrest (car r))
				(loop 2 req opt '() (cdr r)) )
			      (err "invalid syntax of `#!rest' argument") ) 
			  (err "`#!rest' argument marker in wrong context") ) ]
		     [(#!key)
		      (if (not rvar) (set! rvar (macro-alias 'tmp se)))
		      (if (fx<= mode 2)
			  (loop 3 req opt '() r)
			  (err "`#!key' argument marker in wrong context") ) ]
		     [else
		      (cond [(symbol? var)
			     (case mode
			       [(0) (loop 0 (cons var req) '() '() r)]
			       [(1) (loop 1 req (cons (list var #f) opt) '() r)]
			       [(2) (err "invalid lambda list syntax after `#!rest' marker")]
			       [else (loop 3 req opt (cons (list var) key) r)] ) ]
			    [(and (list? var) (eq? 2 (length var)) (symbol? (car var)))
			     (case mode
			       [(0) (err "invalid required argument syntax")]
			       [(1) (loop 1 req (cons var opt) '() r)]
			       [(2) (err "invalid lambda list syntax after `#!rest' marker")]
			       [else (loop 3 req opt (cons var key) r)] ) ]
			    [else (err "invalid lambda list syntax")] ) ] ) ) ] ) ) ) ) ) )


;;; Error message for redefinition of currently used defining form
;
; (i.e.`"(define define ...)")

(define (##sys#defjam-error form)
  (##sys#syntax-error-hook
   "redefinition of currently used defining form" ; help me find something better
   form))


;;; Expansion of bodies (and internal definitions)
;
; This code is disgustingly complex.

(define ##sys#define-definition)
(define ##sys#define-syntax-definition)
(define ##sys#define-values-definition)

(define ##sys#canonicalize-body
  (lambda (body #!optional (se (##sys#current-environment)) cs?)
    (define (comp s id)
      (let ((f (lookup id se)))
	(or (eq? s f)
	    (case s
	      ((define) (if f (eq? f ##sys#define-definition) (eq? s id)))
	      ((define-syntax) (if f (eq? f ##sys#define-syntax-definition) (eq? s id)))
	      ((define-values) (if f (eq? f ##sys#define-values-definition) (eq? s id)))
	      (else (eq? s id))))))
    (define (fini vars vals mvars mvals body)
      (if (and (null? vars) (null? mvars))
	  (let loop ([body2 body] [exps '()])
	    (if (not (pair? body2)) 
		(cons 
		 '##core#begin
		 body) ; no more defines, otherwise we would have called `expand'
		(let ([x (car body2)])
		  (if (and (pair? x) 
			   (let ((d (car x)))
			     (and (symbol? d)
				  (or (comp 'define d)
				      (comp 'define-values d)))))
		      (cons
		       '##core#begin
		       (##sys#append (reverse exps) (list (expand body2))))
		      (loop (cdr body2) (cons x exps)) ) ) ) )
	  (let* ((vars (reverse vars))
		 (result 
		  `(##core#let
		    ,(##sys#map
		      (lambda (v) (##sys#list v (##sys#list '##core#undefined))) 
		      (apply ##sys#append vars mvars) )
		    ,@(map (lambda (v x) `(##core#set! ,v ,x)) vars (reverse vals))
		    ,@(map (lambda (vs x)
			     (let ([tmps (##sys#map gensym vs)])
			       `(##sys#call-with-values
				 (##core#lambda () ,x)
				 (##core#lambda 
				  ,tmps 
				  ,@(map (lambda (v t)
					   `(##core#set! ,v ,t)) 
					 vs tmps) ) ) ) ) 
			   (reverse mvars)
			   (reverse mvals) )
		    ,@body) ) )
	    (dd `(BODY: ,result))
	    result)))
    (define (fini/syntax vars vals mvars mvals body)
      (fini
       vars vals mvars mvals
       (let loop ((body body) (defs '()) (done #f))
	 (cond (done `((##core#letrec-syntax
			,(map cdr (reverse defs)) ,@body) ))
	       ((not (pair? body)) (loop body defs #t))
	       ((and (list? (car body))
		     (>= 3 (length (car body))) 
		     (symbol? (caar body))
		     (comp 'define-syntax (caar body)))
		(let ((def (car body)))
		  (loop 
		   (cdr body) 
		   (cons (cond ((pair? (cadr def)) ; DEPRECATED
				`(define-syntax ; (the first element is actually ignored)
				   ,(caadr def)
				   (##sys#er-transformer
				    (##core#lambda ,(cdadr def) ,@(cddr def)))))
			       ;; insufficient, if introduced by different expansions, but
			       ;; better than nothing:
			       ((eq? (car def) (cadr def))
				(##sys#defjam-error def))
			       (else def))
			 defs) 
		   #f)))
	       (else (loop body defs #t))))))
    (define (expand body)
      (let loop ([body body] [vars '()] [vals '()] [mvars '()] [mvals '()])
	(if (not (pair? body))
	    (fini vars vals mvars mvals body)
	    (let* ((x (car body))
		   (rest (cdr body))
		   (exp1 (and (pair? x) (car x)))
		   (head (and exp1 (symbol? exp1) exp1)))
	      (if (not (symbol? head))
		  (fini vars vals mvars mvals body)
		  (cond
		   ((comp 'define head)
		     (##sys#check-syntax 'define x '(_ _ . #(_ 0)) #f se)
		     (let loop2 ([x x])
		       (let ([head (cadr x)])
			 (cond [(not (pair? head))
				(##sys#check-syntax 'define x '(_ variable . #(_ 0)) #f se)
				(when (eq? (car x) head) ; see above
				  (##sys#defjam-error x))
				(loop rest (cons head vars)
				      (cons (if (pair? (cddr x))
						(caddr x)
						'(##core#undefined) )
					    vals)
				      mvars mvals) ]
			       [(pair? (car head))
				(##sys#check-syntax
				 'define x '(_ (_ . lambda-list) . #(_ 1)) #f se)
				(loop2
				 (##sys#expand-curried-define head (cddr x) se)) ]
			       [else
				(##sys#check-syntax
				 'define x
				 '(_ (variable . lambda-list) . #(_ 1)) #f se)
				(loop rest
				      (cons (car head) vars)
				      (cons `(##core#lambda ,(cdr head) ,@(cddr x)) vals)
				      mvars mvals) ] ) ) ) )
		    ((comp 'define-syntax head)
		     (##sys#check-syntax 'define-syntax x '(_ _ . #(_ 1)) se)
		     (fini/syntax vars vals mvars mvals body) )
		    ((comp 'define-values head)
		     ;;XXX check for any of the variables being `define-values'
		     (##sys#check-syntax 'define-values x '(_ #(_ 0) _) #f se)
		     (loop rest vars vals (cons (cadr x) mvars) (cons (caddr x) mvals)))
		    ((comp '##core#begin head)
		     (loop (##sys#append (cdr x) rest) vars vals mvars mvals) )
		    (else
		     (if (or (memq head vars) (memq head mvars))
			 (fini vars vals mvars mvals body)
			 (let ((x2 (##sys#expand-0 x se cs?)))
			   (if (eq? x x2)
			       (fini vars vals mvars mvals body)
			       (loop (cons x2 rest)
				     vars vals mvars mvals) ) ) ) ) ) ) ) ) ) )
    (expand body) ) )


;;; A simple expression matcher

(define match-expression
  (lambda (exp pat vars)
    (let ((env '()))
      (define (mwalk x p)
	(cond ((not (pair? p))
	       (cond ((assq p env) => (lambda (a) (equal? x (cdr a))))
		     ((memq p vars)
		      (set! env (cons (cons p x) env))
		      #t)
		     (else (eq? x p)) ) )
	      ((pair? x)
	       (and (mwalk (car x) (car p))
		    (mwalk (cdr x) (cdr p)) ) )
	      (else #f) ) )
      (and (mwalk exp pat) env) ) ) )


;;; Expand "curried" lambda-list syntax for `define'

(define (##sys#expand-curried-define head body se)
  (let ((name #f))
    (define (loop head body)
      (if (symbol? (car head))
	  (begin
	    (set! name (car head))
	    `(##core#lambda ,(cdr head) ,@body) )
	  (loop (car head) `((##core#lambda ,(cdr head) ,@body)) ) ))
    (let ([exp (loop head body)])
      (list 'define name exp) ) ) )


;;; General syntax checking routine:

(define ##sys#line-number-database #f)
(define ##sys#syntax-error-culprit #f)
(define ##sys#syntax-context '())

(define (##sys#syntax-error-hook . args)
  (apply ##sys#signal-hook #:syntax-error
	 (##sys#strip-syntax args)))

(define ##sys#syntax-error/context
  (lambda (msg arg)
    (define (syntax-imports sym)
      (let loop ((defs (or (##sys#get (##sys#strip-syntax sym) '##core#db) '())))
	(cond ((null? defs) '())
	      ((eq? 'syntax (caar defs))
	       (cons (cadar defs) (loop (cdr defs))))
	      (else (loop (cdr defs))))))		     
    (if (null? ##sys#syntax-context)
	(##sys#syntax-error-hook msg arg)
	(let ((out (open-output-string)))
	  (define (outstr str)
	    (##sys#print str #f out))
	  (let loop ((cx ##sys#syntax-context))
	    (cond ((null? cx)		; no unimported syntax found
		   (outstr msg)
		   (outstr ": ")
		   (##sys#print arg #t out)
		   (outstr "\ninside expression `(")
		   (##sys#print (##sys#strip-syntax (car ##sys#syntax-context)) #t out)
		   (outstr " ...)'"))
		  (else 
		   (let* ((sym (##sys#strip-syntax (car cx)))
			  (us (syntax-imports sym)))
		     (cond ((pair? us)
			    (outstr msg)
			    (outstr ": ")
			    (##sys#print arg #t out)
			    (outstr "\n\n  Perhaps you intended to use the syntax `(")
			    (##sys#print sym #t out)
			    (outstr " ...)' without importing it first.\n")
			    (if (fx= 1 (length us))
				(outstr
				 (string-append
				  "  Suggesting: `(import "
				  (symbol->string (car us))
				  ")'"))
				(outstr
				 (string-append
				  "  Suggesting one of:\n"
				  (let loop ((lst us))
				    (if (null? lst)
					""
					(string-append
					 "\n      (import " (symbol->string (car lst)) ")'"
					 (loop (cdr lst)))))))))
			   (else (loop (cdr cx))))))))
	  (##sys#syntax-error-hook (get-output-string out))))))

(define syntax-error ##sys#syntax-error-hook)

(define (##sys#syntax-rules-mismatch input)
  (##sys#syntax-error-hook "no rule matches form" input))

(define (get-line-number sexp)
  (and ##sys#line-number-database
       (pair? sexp)
       (let ([head (car sexp)])
	 (and (symbol? head)
	      (cond [(##sys#hash-table-ref ##sys#line-number-database head)
		     => (lambda (pl)
			  (let ([a (assq sexp pl)])
			    (and a (cdr a)) ) ) ]
		    [else #f] ) ) ) ) )

(define-constant +default-argument-count-limit+ 99999)

(define ##sys#check-syntax
  (lambda (id exp pat #!optional culprit (se (##sys#current-environment)))

    (define (test x pred msg)
      (unless (pred x) (err msg)) )

    (define (err msg)
      (let* ([sexp ##sys#syntax-error-culprit]
	     [ln (get-line-number sexp)] )
	(##sys#syntax-error-hook
	 (if ln 
	     (string-append "(" ln ") in `" (symbol->string id) "' - " msg)
	     (string-append "in `" (symbol->string id) "' - " msg) )
	 exp) ) )

    (define (lambda-list? x)
      (or (##sys#extended-lambda-list? x)
	  (let loop ((x x))
	    (cond ((null? x))
		  ((symbol? x) (not (keyword? x)))
		  ((pair? x)
		   (let ((s (car x)))
		     (and (symbol? s) (not (keyword? s))
			  (loop (cdr x)) ) ) )
		  (else #f) ) ) ) )

    (define (proper-list? x)
      (let loop ((x x))
	(cond ((eq? x '()))
	      ((pair? x) (loop (cdr x)))
	      (else #f) ) ) )

    (when culprit (set! ##sys#syntax-error-culprit culprit))
    (let walk ((x exp) (p pat))
      (cond ((vector? p)
	     (let* ((p2 (vector-ref p 0))
		    (vlen (##sys#size p))
		    (min (if (fx> vlen 1) 
			     (vector-ref p 1)
			     0) )
		    (max (cond ((eq? vlen 1) 1)
			       ((fx> vlen 2) (vector-ref p 2))
			       (else +default-argument-count-limit+) ) ) )
	       (do ((x x (cdr x))
		    (n 0 (fx+ n 1)) )
		   ((eq? x '())
		    (if (fx< n min)
			(err "not enough arguments") ) )
		 (cond ((fx>= n max) 
			(err "too many arguments") )
		       ((not (pair? x))
			(err "not a proper list") )
		       (else (walk (car x) p2) ) ) ) ) )
	    ((##sys#immediate? p)
	     (if (not (eq? p x)) (err "unexpected object")) )
	    ((symbol? p)
	     (case p
	       ((_) #t)
	       ((pair) (test x pair? "pair expected"))
	       ((variable) (test x symbol? "identifier expected"))
	       ((symbol) (test x symbol? "symbol expected"))
	       ((list) (test x proper-list? "proper list expected"))
	       ((number) (test x number? "number expected"))
	       ((string) (test x string? "string expected"))
	       ((lambda-list) (test x lambda-list? "lambda-list expected"))
	       (else
		(test
		 x
		 (lambda (y)
		   (let ((y2 (and (symbol? y) (lookup y se))))
		     (eq? (if (symbol? y2) y2 y) p)))
		 "missing keyword")) ) )
	    ((not (pair? p))
	     (err "incomplete form") )
	    ((not (pair? x)) (err "pair expected"))
	    (else
	     (walk (car x) (car p))
	     (walk (cdr x) (cdr p)) ) ) ) ) )


;;; explicit/implicit-renaming transformer

(define (make-er/ir-transformer handler explicit-renaming?) 
  (##sys#make-structure 
   'transformer
   (lambda (form se dse)
     (let ((renv '()))	  ; keep rename-environment for this expansion
       (assert (list? se) "not a list" se) ;XXX remove later
       (define (rename sym)
	 (cond ((pair? sym)
		(cons (rename (car sym)) (rename (cdr sym))))
	       ((vector? sym)
		(list->vector (rename (vector->list sym))))
	       ((not (symbol? sym)) sym)
	       ((assq sym renv) => 
		(lambda (a) 
		  (dd `(RENAME/RENV: ,sym --> ,(cdr a)))
		  (cdr a)))
	       ((lookup sym se) => 
		(lambda (a)
		  (cond ((symbol? a)
			 ;; Add an extra level of indirection for already aliased
			 ;; symbols.  This prevents aliased symbols from popping up
			 ;; in syntax-stripped output.
			 (cond ((or (getp a '##core#aliased)
				    (getp a '##core#primitive))
				(let ((a2 (macro-alias sym se)))
				  (dd `(RENAME/LOOKUP/ALIASED: ,sym --> ,a ==> ,a2))
				  (set! renv (cons (cons sym a2) renv))
				  a2))
			       (else (dd `(RENAME/LOOKUP: ,sym --> ,a))
				     (set! renv (cons (cons sym a) renv))
				     a)))
			(else
			 (let ((a2 (macro-alias sym se)))
			   (dd `(RENAME/LOOKUP/MACRO: ,sym --> ,a2))
			   (set! renv (cons (cons sym a2) renv))
			   a2)))))
	       (else
		(let ((a (macro-alias sym se)))
		  (dd `(RENAME: ,sym --> ,a))
		  (set! renv (cons (cons sym a) renv))
		  a))))
       (define (compare s1 s2)
	 (let ((result
		(cond ((pair? s1)
		       (and (pair? s2)
			    (compare (car s1) (car s2))
			    (compare (cdr s1) (cdr s2))))
		      ((vector? s1)
		       (and (vector? s2)
			    (let ((len (vector-length s1)))
			      (and (fx= len (vector-length s2))
				   (do ((i 0 (fx+ i 1))
					(f #t (compare (vector-ref s1 i) (vector-ref s2 i))))
				       ((or (fx>= i len) (not f)) f))))))
		      ((and (symbol? s1) (symbol? s2))
		       (let ((ss1 (or (getp s1 '##core#macro-alias)
				      (lookup2 1 s1 dse)
				      s1) )
			     (ss2 (or (getp s2 '##core#macro-alias)
				      (lookup2 2 s2 dse)
				      s2) ) )
			 (cond ((symbol? ss1)
				(cond ((symbol? ss2) 
				       (eq? (or (getp ss1 '##core#primitive) ss1)
					    (or (getp ss2 '##core#primitive) ss2)))
				      ((assq ss1 (##sys#macro-environment)) =>
				       (lambda (a) (eq? (cdr a) ss2)))
				      (else #f) ) )
			       ((symbol? ss2)
				(cond ((assq ss2 (##sys#macro-environment)) =>
				       (lambda (a) (eq? ss1 (cdr a))))
				      (else #f)))
			       (else (eq? ss1 ss2)))))
		      (else (eq? s1 s2))) ) ) 
	   (dd `(COMPARE: ,s1 ,s2 --> ,result)) 
	   result))
       (define (lookup2 n sym dse)
	 (let ((r (lookup sym dse)))
	   (dd "  (lookup/DSE " (list n) ": " sym " --> " 
	       (if (and r (pair? r))
		   '<macro>
		   r)
	       ")")
	   r))
       (define (assq-reverse s l)
	 (cond
	  ((null? l) #f)
	  ((eq? (cdar l) s) (car l))
	  (else (assq-reverse s (cdr l)))))
       (define (mirror-rename sym)
	 (cond ((pair? sym)
		(cons (mirror-rename (car sym)) (mirror-rename (cdr sym))))
	       ((vector? sym)
		(list->vector (mirror-rename (vector->list sym))))
	       ((not (symbol? sym)) sym)
	       (else		 ; Code stolen from ##sys#strip-syntax
		(let ((renamed (lookup sym se) ) )
		  (cond ((assq-reverse sym renv) =>
			 (lambda (a)
			   (dd "REVERSING RENAME: " sym " --> " (car a)) (car a)))
			((not renamed)
			 (dd "IMPLICITLY RENAMED: " sym) (rename sym))
			((pair? renamed)
			 (dd "MACRO: " sym) (rename sym))
			((getp sym '##core#real-name) =>
			 (lambda (name)
			   (dd "STRIP SYNTAX ON " sym " ---> " name)
			   name))
                        ;; Rename builtin aliases so strip-syntax can still
                        ;; access symbols as entered by the user
			(else (let ((implicitly-renamed (rename sym)))
                                (dd "BUILTIN ALIAS: " sym " as " renamed
                                    " --> " implicitly-renamed)
                                implicitly-renamed)))))))
       (if explicit-renaming?
	   ;; Let the user handle renaming
	   (handler form rename compare)
	   ;; Implicit renaming:
	   ;; Rename everything in the input first, feed it to the transformer
	   ;; and then swap out all renamed identifiers by their non-renamed
	   ;; versions, and vice versa.  User can decide when to inject code
	   ;; unhygienically this way.
	   (mirror-rename (handler (rename form) rename compare)) ) ) )))

(define (##sys#er-transformer handler) (make-er/ir-transformer handler #t))
(define (##sys#ir-transformer handler) (make-er/ir-transformer handler #f))

(define er-macro-transformer ##sys#er-transformer)
(define ir-macro-transformer ##sys#ir-transformer)


;;; Macro definitions:

(define (##sys#mark-primitive prims)
  (for-each
   (lambda (a) (putp (cdr a) '##core#primitive (car a)))
   prims))

(##sys#extend-macro-environment
 'import '() 
 (##sys#er-transformer 
  (cut ##sys#expand-import <> <> <> ##sys#current-environment ##sys#macro-environment
       #f #f 'import) ) )

(##sys#extend-macro-environment
 'import-for-syntax '() 
 (##sys#er-transformer 
  (cut ##sys#expand-import <> <> <> ##sys#current-meta-environment 
       ##sys#meta-macro-environment 
       #t #f 'import-for-syntax) ) )

(##sys#extend-macro-environment
 'reexport '() 
 (##sys#er-transformer 
  (cut ##sys#expand-import <> <> <> ##sys#current-environment ##sys#macro-environment 
       #f #t 'reexport) ) )

;; contains only "import[-for-syntax]" and "reexport"
(define ##sys#initial-macro-environment (##sys#macro-environment))

(##sys#extend-macro-environment
 'lambda
 '()
 (##sys#er-transformer
  (lambda (x r c)
    (##sys#check-syntax 'lambda x '(_ lambda-list . #(_ 1)))
    `(##core#lambda ,@(cdr x)))))

(##sys#extend-macro-environment
 'quote
 '()
 (##sys#er-transformer
  (lambda (x r c)
    (##sys#check-syntax 'quote x '(_ _))
    `(##core#quote ,(cadr x)))))

(##sys#extend-macro-environment
 'syntax
 '()
 (##sys#er-transformer
  (lambda (x r c)
    (##sys#check-syntax 'syntax x '(_ _))
    `(##core#syntax ,(cadr x)))))

(##sys#extend-macro-environment
 'if
 '()
 (##sys#er-transformer
  (lambda (x r c)
    (##sys#check-syntax 'if x '(_ _ _ . #(_)))
    `(##core#if ,@(cdr x)))))

(##sys#extend-macro-environment
 'begin
 '()
 (##sys#er-transformer
  (lambda (x r c)
    (##sys#check-syntax 'begin x '(_ . #(_ 0)))
    `(##core#begin ,@(cdr x)))))

(set! ##sys#define-definition
  (##sys#extend-macro-environment
   'define
   '()
   (##sys#er-transformer
    (lambda (x r c)
      (##sys#check-syntax 'define x '(_ . #(_ 1)))
      (let loop ((form x))
	(let ((head (cadr form))
	      (body (cddr form)) )
	  (cond ((not (pair? head))
		 (##sys#check-syntax 'define form '(_ symbol . #(_ 0 1)))
                 (let ((name (or (getp head '##core#macro-alias) head)))
                   (##sys#register-export name (##sys#current-module)))
		 (when (c (r 'define) head)
		   (##sys#defjam-error x))
		 `(##core#set! 
		   ,head 
		   ,(if (pair? body) (car body) '(##core#undefined))) )
		((pair? (car head))
		 (##sys#check-syntax 'define form '(_ (_ . lambda-list) . #(_ 1)))
		 (loop (##sys#expand-curried-define head body '())) ) ;XXX '() should be se
		(else
		 (##sys#check-syntax 'define form '(_ (symbol . lambda-list) . #(_ 1)))
		 (loop (list (car x) (car head) `(##core#lambda ,(cdr head) ,@body)))))))))))

(set! ##sys#define-syntax-definition
  (##sys#extend-macro-environment
   'define-syntax
   '()
   (##sys#er-transformer
    (lambda (form r c)
      (let ((head (cadr form))
	    (body (cddr form)) )
	(cond ((not (pair? head))
	       (##sys#check-syntax 'define-syntax head 'symbol)
	       (##sys#check-syntax 'define-syntax body '#(_ 1))
               (let ((name (or (getp head '##core#macro-alias) head)))
                 (##sys#register-export name (##sys#current-module)))
	       (when (c (r 'define-syntax) head)
		 (##sys#defjam-error form))
	       `(##core#define-syntax ,head ,(car body)))
	      (else			; DEPRECATED
	       (##sys#check-syntax 'define-syntax head '(_ . lambda-list))
	       (##sys#check-syntax 'define-syntax body '#(_ 1))
	       (when (eq? (car form) (car head))
		 (##sys#syntax-error-hook
		  "redefinition of `define-syntax' not allowed in syntax-definition"
		  form))
	       `(##core#define-syntax 
		 ,(car head)
		 (##sys#er-transformer (##core#lambda ,(cdr head) ,@body))))))))))

(##sys#extend-macro-environment
 'let
 '()
 (##sys#er-transformer
  (lambda (x r c)
    (if (and (pair? (cdr x)) (symbol? (cadr x)))
	(##sys#check-syntax 'let x '(_ symbol #((symbol _) 0) . #(_ 1)))
	(##sys#check-syntax 'let x '(_ #((symbol _) 0) . #(_ 1))))
    `(##core#let ,@(cdr x)))))

(##sys#extend-macro-environment
 'letrec
 '()
 (##sys#er-transformer
  (lambda (x r c)
    (##sys#check-syntax 'letrec x '(_ #((symbol _) 0) . #(_ 1)))
    `(##core#letrec ,@(cdr x)))))

(##sys#extend-macro-environment
 'let-syntax
 '()
 (##sys#er-transformer
  (lambda (x r c)
    (##sys#check-syntax 'let-syntax x '(_ #((symbol _) 0) . #(_ 1)))
    `(##core#let-syntax ,@(cdr x)))))

(##sys#extend-macro-environment
 'letrec-syntax
 '()
 (##sys#er-transformer
  (lambda (x r c)
    (##sys#check-syntax 'letrec-syntax x '(_ #((symbol _) 0) . #(_ 1)))
    `(##core#letrec-syntax ,@(cdr x)))))

(##sys#extend-macro-environment
 'set!
 '()
 (##sys#er-transformer
  (lambda (x r c)
    (##sys#check-syntax 'set! x '(_ _ _))
    (let ((dest (cadr x))
	  (val (caddr x)))
      (cond ((pair? dest)
	     `((##sys#setter ,(car dest)) ,@(cdr dest) ,val))
	    (else `(##core#set! ,dest ,val)))))))

(##sys#extend-macro-environment
 'and
 '()
 (##sys#er-transformer
  (lambda (form r c)
    (let ((body (cdr form)))
      (if (null? body)
	  #t
	  (let ((rbody (cdr body))
		(hbody (car body)) )
	    (if (null? rbody)
		hbody
		`(##core#if ,hbody (,(r 'and) ,@rbody) #f) ) ) ) ) ) ) )

(##sys#extend-macro-environment
 'or 
 '()
 (##sys#er-transformer
  (lambda (form r c)
    (let ((body (cdr form)))
     (if (null? body)
	 #f
	 (let ((rbody (cdr body))
	       (hbody (car body)))
	   (if (null? rbody)
	       hbody
	       (let ((tmp (r 'tmp)))
		 `(##core#let ((,tmp ,hbody))
		    (##core#if ,tmp ,tmp (,(r 'or) ,@rbody)) ) ) ) ) ) ) ) ) )

(##sys#extend-macro-environment
 'cond
 '()
 (##sys#er-transformer
  (lambda (form r c)
    (let ((body (cdr form))
	  (%=> (r '=>))
	  (%or (r 'or))
	  (%else (r 'else)))
      (let expand ((clauses body) (else? #f))
	(if (not (pair? clauses))
	    '(##core#undefined)
	    (let ((clause (car clauses))
		  (rclauses (cdr clauses)) )
	      (##sys#check-syntax 'cond clause '#(_ 1))
	      (cond ((c %else (car clause))
		     (expand rclauses #t)
		     `(##core#begin ,@(cdr clause)))
		    (else?
		     (##sys#warn
		      "non-`else' clause following `else' clause in `cond'"
		      (##sys#strip-syntax clause))
		     (expand rclauses #t)
		     '(##core#begin))
		    ((null? (cdr clause)) 
		     `(,%or ,(car clause) ,(expand rclauses #f)))
		    ((c %=> (cadr clause))
		     (let ((tmp (r 'tmp)))
		       `(##core#let ((,tmp ,(car clause)))
				    (##core#if ,tmp
					       (,(caddr clause) ,tmp)
					       ,(expand rclauses #f) ) ) ) )
		    ((and (list? clause) (fx= (length clause) 4)
			  (c %=> (caddr clause)))
		     (let ((tmp (r 'tmp)))
		       `(##sys#call-with-values
			 (##core#lambda () ,(car clause))
			 (##core#lambda 
			  ,tmp
			  (if (##sys#apply ,(cadr clause) ,tmp)
			      (##sys#apply ,(cadddr clause) ,tmp)
			      ,(expand rclauses #f) ) ) ) ) )
		    (else `(##core#if ,(car clause) 
				      (##core#begin ,@(cdr clause))
				      ,(expand rclauses #f) ) ) ) ) ) ) ) ) ) )

(##sys#extend-macro-environment
 'case
 '()
 (##sys#er-transformer
  (lambda (form r c)
    (##sys#check-syntax 'case form '(_ _ . #(_ 0)))
    (let ((exp (cadr form))
	  (body (cddr form)) )
      (let ((tmp (r 'tmp))
	    (%or (r 'or))
	    (%eqv? (r 'eqv?))
	    (%else (r 'else)))
	`(let ((,tmp ,exp))
	   ,(let expand ((clauses body) (else? #f))
	      (if (not (pair? clauses))
		  '(##core#undefined)
		  (let ((clause (car clauses))
			(rclauses (cdr clauses)) )
		    (##sys#check-syntax 'case clause '#(_ 1))
		    (cond ((c %else (car clause))
			   (expand rclauses #t)
			   `(##core#begin ,@(cdr clause)) )
			  (else?
			   (##sys#notice
			    "non-`else' clause following `else' clause in `case'"
			    (##sys#strip-syntax clause))
			   (expand rclauses #t)
			   '(##core#begin))
			  (else
			   `(##core#if (,%or ,@(##sys#map
						(lambda (x) `(,%eqv? ,tmp ',x))
						(car clause)))
				       (##core#begin ,@(cdr clause)) 
				       ,(expand rclauses #f) ) ) ) ) ) ) ) ) ) ) ) )

(##sys#extend-macro-environment
 'let*
 '()
 (##sys#er-transformer
  (lambda (form r c)
    (##sys#check-syntax 'let* form '(_ #((symbol _) 0) . #(_ 1)))
    (let ((bindings (cadr form))
	  (body (cddr form)) )
      (let expand ((bs bindings))
	(if (eq? bs '())
	    `(##core#let () ,@body)
	    `(##core#let (,(car bs)) ,(expand (cdr bs))) ) ) ) ) ) )

(##sys#extend-macro-environment
 'do
 '()
 (##sys#er-transformer
  (lambda (form r c)
    (##sys#check-syntax 'do form '(_ #((symbol _ . #(_)) 0) . #(_ 1)))
    (let ((bindings (cadr form))
	  (test (caddr form))
	  (body (cdddr form))
	  (dovar (r 'doloop)))
      `(##core#let 
	,dovar
	,(##sys#map (lambda (b) (list (car b) (car (cdr b)))) bindings)
	(##core#if ,(car test)
		   ,(let ((tbody (cdr test)))
		      (if (eq? tbody '())
			  '(##core#undefined)
			  `(##core#begin ,@tbody) ) )
		   (##core#begin
		    ,(if (eq? body '())
			 '(##core#undefined)
			 `(##core#let () ,@body) )
		    (##core#app
		     ,dovar ,@(##sys#map (lambda (b) 
					   (if (eq? (cdr (cdr b)) '())
					       (car b)
					       (car (cdr (cdr b))) ) )
					 bindings) ) ) ) ) ) ) ) )

(##sys#extend-macro-environment
 'quasiquote
 '()
 (##sys#er-transformer
  (lambda (form r c)
    (let ((%quasiquote (r 'quasiquote))
	  (%unquote (r 'unquote))
	  (%unquote-splicing (r 'unquote-splicing)))
      (define (walk x n) (simplify (walk1 x n)))
      (define (walk1 x n)
	(cond ((vector? x)
	       `(##sys#list->vector ,(walk (vector->list x) n)) )
	      ((not (pair? x)) `(##core#quote ,x))
	      (else
	       (let ((head (car x))
		     (tail (cdr x)))
		 (cond ((c %unquote head)
                        (cond ((eq? n 0)
                               (##sys#check-syntax 'unquote x '(_ _))
                               (car tail))
                              (else (list '##sys#cons `(##core#quote ,%unquote)
                                          (walk tail (fx- n 1)) ) )))
		       ((c %quasiquote head)
			(list '##sys#cons `(##core#quote ,%quasiquote) 
                              (walk tail (fx+ n 1)) ) )
		       ((and (pair? head) (c %unquote-splicing (car head)))
                        (cond ((eq? n 0)
                               (##sys#check-syntax 'unquote-splicing head '(_ _))
                               `(##sys#append ,(cadr head) ,(walk tail n)))
                              (else
                               `(##sys#cons
                                 (##sys#cons (##core#quote ,%unquote-splicing)
                                             ,(walk (cdr head) (fx- n 1)) )
                                 ,(walk tail n)))))
		       (else
			`(##sys#cons ,(walk head n) ,(walk tail n)) ) ) ) ) ) )
      (define (simplify x)
	(cond ((match-expression x '(##sys#cons a (##core#quote ())) '(a))
	       => (lambda (env) (simplify `(##sys#list ,(cdr (assq 'a env))))) )
	      ((match-expression x '(##sys#cons a (##sys#list . b)) '(a b))
	       => (lambda (env)
		    (let ((bxs (assq 'b env)))
		      (if (fx< (length bxs) 32)
			  (simplify `(##sys#list ,(cdr (assq 'a env))
						 ,@(cdr bxs) ) ) 
			  x) ) ) )
	      ((match-expression x '(##sys#append a (##core#quote ())) '(a))
	       => (lambda (env) (cdr (assq 'a env))) )
	      (else x) ) )
      (##sys#check-syntax 'quasiquote form '(_ _))
      (walk (cadr form) 0) ) ) ) )

(##sys#extend-macro-environment
 'delay
 '()
 (##sys#er-transformer
  (lambda (form r c)
    (##sys#check-syntax 'delay form '(_ _))
    `(##sys#make-promise (##core#lambda () ,(cadr form))))))

(##sys#extend-macro-environment
 'cond-expand
 '()
 (##sys#er-transformer
  (lambda (form r c)
    (let ((clauses (cdr form))
	  (%or (r 'or))
	  (%not (r 'not))
	  (%else (r 'else))
	  (%and (r 'and)))
      (define (err x) 
	(##sys#error "syntax error in `cond-expand' form"
		     x
		     (cons 'cond-expand clauses)) )
      (define (test fx)
	(cond ((symbol? fx) (##sys#feature? (##sys#strip-syntax fx)))
	      ((not (pair? fx)) (err fx))
	      (else
	       (let ((head (car fx))
		     (rest (cdr fx)))
		 (cond ((c %and head)
			(or (eq? rest '())
			    (if (pair? rest)
				(and (test (car rest))
				     (test `(,%and ,@(cdr rest))) )
				(err fx) ) ) )
		       ((c %or head)
			(and (not (eq? rest '()))
			     (if (pair? rest)
				 (or (test (car rest))
				     (test `(,%or ,@(cdr rest))) )
				 (err fx) ) ) )
		       ((c %not head) (not (test (cadr fx))))
		       (else (err fx)) ) ) ) ) )
      (let expand ((cls clauses))
	(cond ((eq? cls '())
	       (##sys#apply
		##sys#error "no matching clause in `cond-expand' form" 
		(map (lambda (x) (car x)) clauses) ) )
	      ((not (pair? cls)) (err cls))
	      (else
	       (let ((clause (car cls))
		    (rclauses (cdr cls)) )
		 (if (not (pair? clause)) 
		     (err clause)
		     (let ((id (car clause)))
		       (cond ((c id %else)
			      (let ((rest (cdr clause)))
				(if (eq? rest '())
				    '(##core#undefined)
				    `(##core#begin ,@rest) ) ) )
			     ((test id) `(##core#begin ,@(cdr clause)))
			     (else (expand rclauses)) ) ) ) ) ) ) ) ) ) ) )

(##sys#extend-macro-environment
 'require-library
 '()
 (##sys#er-transformer
  (lambda (x r c)
    (let ((ids (cdr x)))
      `(##core#require-extension ,ids #f) ) ) ) )

(##sys#extend-macro-environment
 'require-extension
 '()
 (##sys#er-transformer
  (lambda (x r c)
    (let ((ids (cdr x)))
      `(##core#require-extension ,ids #t) ) ) ) )

(##sys#extend-macro-environment
 'module
 '()
 (##sys#er-transformer
  (lambda (x r c)
    (let ((len (length x)))
      (##sys#check-syntax 'module x '(_ symbol _ . #(_ 0)))
      (cond ((and (fx>= len 4) (c (r '=) (caddr x)))
	     (let* ((x (##sys#strip-syntax x))
		    (name (cadr x))
		    (app (cadddr x)))
	       (cond ((symbol? app)
		      (cond ((fx> len 4)
			     ;; feature suggested by syn:
			     ;;
			     ;; (module NAME = FUNCTORNAME BODY ...)
			     ;; ~>
			     ;; (begin
			     ;;   (module _NAME * BODY ...)
			     ;;   (module NAME = (FUNCTORNAME _NAME)))
			     ;;
			     ;; - the use of "_NAME" is a bit stupid, but it must be
			     ;;   externally visible to generate an import library from
			     ;;   and compiling "NAME" separately may need an import-lib
			     ;;   for stuff in "BODY" (say, syntax needed by syntax exported
			     ;;   from the functor, or something like this...)
			     (let ((mtmp (string->symbol 
					  (##sys#string-append 
					   "_"
					   (symbol->string name))))
				   (%module (r 'module)))
			       `(##core#begin
				 (,%module ,mtmp * ,@(cddddr x))
				 (,%module ,name = (,app ,mtmp)))))
			    (else
			     (##sys#register-module-alias name app)
			     '(##core#undefined))))
		     (else
		      (##sys#check-syntax 
		       'module x '(_ symbol _ (symbol . #(_ 1))))
		      (##sys#instantiate-functor
		       name
		       (car app)	; functor name
		       (cdr app))))))	; functor arguments
	    (else
	     ;;XXX use module name in "loc" argument?
	     (let ((exports
		    (##sys#validate-exports
		     (##sys#strip-syntax (caddr x)) 'module)))
	       `(##core#module 
		 ,(cadr x)
		 ,(if (eq? '* exports)
		      #t 
		      exports)
		 ,@(let ((body (cdddr x)))
		     (if (and (pair? body) 
			      (null? (cdr body))
			      (string? (car body)))
			 `((##core#include ,(car body)))
			 body))))))))))

(##sys#extend-macro-environment
 'begin-for-syntax
 '()
 (##sys#er-transformer
  (lambda (x r c)
    (##sys#check-syntax 'begin-for-syntax x '(_ . #(_ 0)))
    (##sys#register-meta-expression `(##core#begin ,@(cdr x)))
    `(##core#elaborationtimeonly (##core#begin ,@(cdr x))))))

(##sys#extend-macro-environment
 'export
 '()
 (##sys#er-transformer
  (lambda (x r c)
    (let ((exps 
	   (##sys#validate-exports 
	    (##sys#strip-syntax (cdr x))
	    'export))
	  (mod (##sys#current-module)))
      (when mod
	(##sys#add-to-export-list mod exps))
      '(##core#undefined)))))


;;; syntax-rules

(include "synrules.scm")


;;; the base macro environment ("scheme", essentially)

(define (##sys#macro-subset me0 #!optional parent-env)
  (let ((se (let loop ((me (##sys#macro-environment)))
	      (if (or (null? me) (eq? me me0))
		  '()
		  (cons (car me) (loop (cdr me)))))))
    (##sys#fixup-macro-environment se parent-env)))

(define (##sys#fixup-macro-environment se #!optional parent-env)
  (let ((se2 (if parent-env (##sys#append se parent-env) se)))
    (for-each				; fixup se
     (lambda (sdef)
       (when (pair? (cdr sdef))
	 (set-car!
	  (cdr sdef) 
	  (if (null? (cadr sdef)) 
	      se2
	      (##sys#append (cadr sdef) se2)))))
     se)
    se))

(define ##sys#default-macro-environment
  (##sys#fixup-macro-environment (##sys#macro-environment)))

(define ##sys#meta-macro-environment (make-parameter (##sys#macro-environment)))
