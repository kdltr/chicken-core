;;;; scrutinizer.scm - The CHICKEN Scheme compiler (local flow analysis)
;
; Copyright (c) 2009-2018, The CHICKEN Team
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
  (unit scrutinizer)
  (uses data-structures expand extras pathname port support internal))

(module chicken.compiler.scrutinizer
    (scrutinize load-type-database emit-types-file
     validate-type check-and-validate-type install-specializations
     ;; Exported for use in the tests:
     match-types refine-types type<=?)

(import scheme
	chicken.base
	chicken.compiler.support
	chicken.fixnum
	chicken.format
	chicken.internal
	chicken.io
	chicken.pathname
	chicken.platform
	chicken.port
	chicken.pretty-print
	chicken.string
	chicken.syntax)

(include "tweaks")
(include "mini-srfi-1.scm")

(define d-depth 0)
(define scrutiny-debug #t)

(define (d fstr . args)
  (when (and scrutiny-debug (##sys#debug-mode?))
    (printf "[debug|~a] ~a~?~%" d-depth (make-string d-depth #\space) fstr args)) )

(define dd d)
(define ddd d)

(define-syntax d (syntax-rules () ((_ . _) (void))))
(define-syntax dd (syntax-rules () ((_ . _) (void))))
(define-syntax ddd (syntax-rules () ((_ . _) (void))))


;;; Walk node tree, keeping type and binding information
;
; result specifiers:
;
;   SPEC = * | (TYPE1 ...)
;   TYPE = (or TYPE1 ...)
;        | (not TYPE)
;        | (struct NAME)
;        | (procedure [NAME] (TYPE1 ... [#!optional TYPE1 ...] [#!rest [TYPE | values]]) . RESULTS)
;        | VALUE
;        | BASIC
;        | COMPLEX
;        | (forall (TVAR1 ...) TYPE)
;        | (refine (SYMBOL ...) VALUE)
;        | deprecated
;        | (deprecated NAME)
;   VALUE = string | symbol | char | number | boolean | true | false |
;           null | eof | blob |  pointer | port | locative | fixnum |
;           float | bignum | ratnum | cplxnum | integer | pointer-vector
;   BASIC = * | list | pair | procedure | vector | undefined | noreturn | values
;   COMPLEX = (pair TYPE TYPE)
;           | (vector-of TYPE)
;           | (list-of TYPE)
;           | (vector TYPE1 ...)
;           | (list TYPE1 ...)
;   RESULTS = *
;           | (TYPE1 ...)
;   TVAR = (VAR TYPE) | VAR
;
; global symbol properties:
;
;   ##compiler#type            ->  TYPESPEC
;   ##compiler#type-source     ->  'db | 'local | 'inference
;   ##compiler#predicate       ->  TYPESPEC
;   ##compiler#specializations ->  (SPECIALIZATION ...)
;   ##compiler#local-specializations ->  (SPECIALIZATION ...)
;   ##compiler#enforce         ->  BOOL
;   ##compiler#special-result-type -> PROCEDURE
;   ##compiler#escape          ->  #f | 'yes | 'no
;   ##compiler#type-abbreviation -> TYPESPEC
;
; specialization specifiers:
;
;   SPECIALIZATION = ((TYPE ... [#!rest TYPE]) [RESULTS] TEMPLATE)
;   TEMPLATE = #(INDEX)
;            | #(INDEX ...)
;            | #(SYMBOL)
;            | INTEGER | SYMBOL | STRING
;            | (quote CONSTANT)
;            | (TEMPLATE . TEMPLATE)
;
; As an alternative to the "#!rest" and "#!optional" keywords, "&rest" or "&optional"
; may be used.


(define-constant +fragment-max-length+ 6)
(define-constant +fragment-max-depth+ 4)
(define-constant +maximal-union-type-length+ 20)
(define-constant +maximal-complex-object-constructor-result-type-length+ 256)

(define-constant value-types
  '(string symbol char null boolean true false blob eof fixnum float number
    integer bignum ratnum cplxnum pointer-vector port pointer locative))

(define-constant basic-types
  '(* list pair procedure vector undefined deprecated noreturn values))

(define-constant struct-types
  '(u8vector s8vector u16vector s16vector u32vector s32vector u64vector
    s64vector f32vector f64vector thread queue environment time
    continuation lock mmap condition hash-table tcp-listener))

(define-constant type-expansions
  '((pair . (pair * *))
    (list . (list-of *))
    (vector . (vector-of *))
    (boolean . (or true false))
    (integer . (or fixnum bignum))
    (number . (or fixnum float bignum ratnum cplxnum))
    (procedure . (procedure (#!rest *) . *))))

(define-inline (struct-type? t)
  (and (pair? t) (eq? (car t) 'struct)))

(define-inline (value-type? t)
  (or (struct-type? t) (memq t value-types)))

(define (type-name x)
  (let ((t (strip-syntax x)))
    (if (refinement-type? t)
	(sprintf "~a-~a" (string-intersperse (map conc (second t)) "/") (third t))
	(sprintf "~a" t))))

(define specialization-statistics '())
(define trail '())

(define (multiples n)
  (if (= n 1) "" "s"))

(define (walked-result n)
  (first (node-parameters n)))		; assumes ##core#the/result node

(define (type-always-immediate? t)
  (cond ((pair? t)
	 (case (car t)
	   ((or) (every type-always-immediate? (cdr t)))
	   ((forall) (type-always-immediate? (third t)))
	   (else #f)))
	((memq t '(eof null fixnum char boolean undefined)) #t)
	(else #f)))

(define (node-source-prefix n)
  (let ((line (node-line-number n)))
    (if (not line) "" (sprintf "(~a) " line))))

(define (location-name loc)
  (define (lname loc1)
    (if loc1
	(sprintf "procedure `~a'" (real-name loc1))
	"unknown procedure"))
  (cond ((null? loc) "at toplevel:\n  ")
	((null? (cdr loc))
	 (sprintf "in toplevel ~a:\n  " (lname (car loc))))
	(else
	 (let rec ((loc loc))
	   (if (null? (cdr loc))
	       (location-name loc)
	       (sprintf "in local ~a,\n  ~a" (lname (car loc)) (rec (cdr loc))))))))

(define (scrutinize node db complain specialize strict block-compilation)
  (let ((blist '())			; (((VAR . FLOW) TYPE) ...)
	(aliased '())
	(noreturn #f)
	(dropped-branches 0)
	(assigned-immediates 0)
	(errors #f)
	(safe-calls 0))

    (define (constant-result lit)
      (cond ((string? lit) 'string)
	    ((symbol? lit) 'symbol)
	    ;; Do not assume fixnum width matches target platforms!
	    ((or (big-fixnum? lit) (small-bignum? lit)) 'integer)
	    ((fixnum? lit) 'fixnum)
	    ((bignum? lit) 'bignum)
	    ((flonum? lit) 'float)	; Why not "flonum", for consistency?
	    ((ratnum? lit) 'ratnum)
	    ((cplxnum? lit) 'cplxnum)
	    ((boolean? lit)
	     (if lit 'true 'false))
	    ((null? lit) 'null)
	    ((list? lit) 
	     `(list ,@(map constant-result lit)))
	    ((pair? lit)
	     (simplify-type
	      `(pair ,(constant-result (car lit)) ,(constant-result (cdr lit)))))
	    ((eof-object? lit) 'eof)
	    ((vector? lit) 
	     (simplify-type
	      `(vector ,@(map constant-result (vector->list lit)))))
	    ((and (not (##sys#immediate? lit)) (##sys#generic-structure? lit))
	     `(struct ,(##sys#slot lit 0)))
	    ((char? lit) 'char)
	    (else '*)))

    (define (global-result id loc)
      (cond ((variable-mark id '##compiler#type) =>
	     (lambda (a)
	       (cond
		((eq? a 'deprecated)
		 (report loc "use of deprecated `~a'" id)
		 '(*))
		((and (pair? a) (eq? (car a) 'deprecated))
		 (report
		  loc
		  "use of deprecated `~a' - consider `~a'"
		  id (cadr a))
		 '(*))
		(else (list a)))))
	    (else '(*))))

    (define (blist-type id flow)
      (cond ((find (lambda (b) 
		     (and (eq? id (caar b))
			  (memq (cdar b) flow)) )
		   blist)
	     => cdr)
	    (else #f)))

    (define (variable-result id e loc flow)
      (cond ((blist-type id flow) => list)
	    ((and (not strict)
		  (db-get db id 'assigned) 
		  (not (variable-mark id '##compiler#type-source)))
	     '(*))
	    ((assq id e) =>
	     (lambda (a)
	       (cond ((eq? 'undefined (cdr a))
		      #;(report
		       loc
		       "access to variable `~a' which has an undefined value"
		       (real-name id db))
		      '(*))
		     (else (list (cdr a))))))
	    (else (global-result id loc))))

    (define (always-true1 t)
      (cond ((pair? t)
	     (case (car t)
	       ((or) (every always-true1 (cdr t)))
	       ((not) (not (always-true1 (second t))))
	       ((forall) (always-true1 (third t)))
	       (else #t)))
	    ((memq t '(* boolean false undefined noreturn)) #f)
	    (else #t)))

    (define (always-true if-node test-node t loc)
      (and-let* ((_ (always-true1 t)))
	(report-notice
	 loc "~aexpected a value of type boolean in conditional, but \
	 was given a value of type `~a' which is always true:~%~%~a"
	 (node-source-prefix test-node) t (pp-fragment if-node))
	#t))

    (define (always-false if-node test-node t loc)
      (and-let* ((_ (eq? t 'false)))
	(report-notice
	 loc "~ain conditional, test expression will always return false:~%~%~a"
	 (node-source-prefix test-node) (pp-fragment if-node))
	#t))

    (define (always-immediate var t loc)
      (and-let* ((_ (type-always-immediate? t)))
	(d "assignment to var ~a in ~a is always immediate" var loc)
	#t))

    (define (single node what tv loc)
      (if (eq? '* tv)
	  '*
	  (let ((n (length tv)))
	    (cond ((= 1 n) (car tv))
		  ((zero? n)
		   (report
		    loc
		    "~aexpected a single result ~a, but received zero results"
		    (node-source-prefix node) what)
		   'undefined)
		  (else
		   (report
		    loc
		    "~aexpected a single result ~a, but received ~a result~a"
		    (node-source-prefix node) what n (multiples n))
		   (first tv))))))

    (define (report-notice loc msg . args)
      (when complain
	(##sys#notice
	 (conc (location-name loc)
	       (sprintf "~?" msg (map type-name args))))))

    (define (report loc msg . args)
      (when complain
	(warning
	 (conc (location-name loc)
	       (sprintf "~?" msg (map type-name args))))))

    (define (report-error loc msg . args)
      (set! errors #t)
      (apply report loc msg args))

    (define add-loc cons)

    (define (fragment x)
      (let ((x (build-expression-tree (source-node-tree x))))
	(let walk ((x x) (d 0))
	  (cond ((atom? x) (strip-syntax x))
		((>= d +fragment-max-depth+) '...)
		((list? x)
		 (let* ((len (length x))
			(xs (if (< +fragment-max-length+ len)
				(append (take x +fragment-max-length+) '(...))
				x)))
		   (map (cute walk <> (add1 d)) xs)))
		(else (strip-syntax x))))))

    (define (pp-fragment x)
      (string-chomp
       (with-output-to-string
	 (lambda ()
	   (pp (fragment x))))))

    (define (get-specializations name)
      (let* ((a (variable-mark name '##compiler#local-specializations))
	     (b (variable-mark name '##compiler#specializations))
	     (c (append (or a '()) (or b '()))))
	(and (pair? c) c)))

    (define (call-result node args e loc params typeenv)
      (define (pname)
	(sprintf "~ain procedure call to `~s', "
		 (node-source-prefix node)
		 (fragment (first (node-subexpressions node)))))
      (let* ((actualtypes (map walked-result args))
	     (ptype (car actualtypes))
	     (pptype? (procedure-type? ptype))
	     (nargs (length (cdr args)))
	     (xptype `(procedure ,(make-list nargs '*) *))
	     (typeenv (append-map type-typeenv actualtypes))
	     (op #f))
	(d "  call: ~a, te: ~a" actualtypes typeenv)
	(cond ((and (not pptype?) (not (match-types xptype ptype typeenv)))
	       (report
		loc
		"~aexpected a value of type `~a' but was given a value of type `~a'"
		(pname)
		(resolve xptype typeenv)
		(resolve ptype typeenv))
	       (values '* #f))
	      (else
	       (let-values (((atypes values-rest ok alen)
			     (procedure-argument-types ptype nargs typeenv)))
		 (unless ok
		   (report
		    loc
		    "~aexpected ~a argument~a but was given ~a argument~a"
		    (pname)
		    alen (multiples alen)
		    nargs (multiples nargs)))
		 (do ((actualtypes (cdr actualtypes) (cdr actualtypes))
		      (atypes atypes (cdr atypes))
		      (i 1 (add1 i)))
		     ((or (null? actualtypes) (null? atypes)))
		   (unless (match-types 
			    (car atypes)
			    (car actualtypes)
			    typeenv)
		     (report
		      loc
		      "~aexpected argument #~a of type `~a' but was given an argument of type `~a'"
		      (pname)
		      i
		      (resolve (car atypes) typeenv)
		      (resolve (car actualtypes) typeenv))))
		 (when (noreturn-procedure-type? ptype)
		   (set! noreturn #t))
		 (let ((r (procedure-result-types ptype values-rest (cdr actualtypes) typeenv)))
		   (let* ((pn (procedure-name ptype))
			  (trail0 trail))
		     (when pn
		       (cond ((and (fx= 1 nargs) 
				   (variable-mark pn '##compiler#predicate)) =>
				   (lambda (pt)
				     (cond ((match-argument-types (list pt) (cdr actualtypes) typeenv)
					    (report-notice
					     loc
					     "~athe predicate is called with an argument of type `~a' \
					      and will always return true"
					     (pname) (cadr actualtypes))
					    (when specialize
					      (specialize-node!
					       node (cdr args)
					       `(let ((#(tmp) #(1))) '#t))
					      (set! r '(true))
					      (set! op (list pn pt))))
					   ((begin
					      (trail-restore trail0 typeenv)
					      (match-argument-types (list `(not ,pt)) (cdr actualtypes) typeenv))
					    (report-notice
					     loc
					     "~athe predicate is called with an argument of type `~a' \
					      and will always return false"
					     (pname) (cadr actualtypes))
					    (when specialize
					      (specialize-node!
					       node (cdr args)
					       `(let ((#(tmp) #(1))) '#f))
					      (set! r '(false))
					      (set! op (list pt `(not ,pt)))))
					   (else (trail-restore trail0 typeenv)))))
			     ((and specialize (get-specializations pn)) =>
			      (lambda (specs)
				(let loop ((specs specs))
				  (and (pair? specs)
				       (let* ((spec (car specs))
					      (stype (first spec))
					      (tenv2 (append
						      (append-map type-typeenv stype)
						      typeenv)))
					 (cond ((match-argument-types stype (cdr actualtypes) tenv2)
						(set! op (cons pn (car spec)))
						(set! typeenv tenv2)
						(let* ((r2 (and (pair? (cddr spec))
								(second spec)))
						       (rewrite (if r2
								    (third spec)
								    (second spec))))
						  (specialize-node! node (cdr args) rewrite)
						  (when r2 (set! r r2))))
					       (else
						(trail-restore trail0 tenv2)
						(loop (cdr specs))))))))))
		       (when op
			 (d "  specialized: `~s' for ~a" (car op) (cdr op))
			 (cond ((assoc op specialization-statistics) =>
				(lambda (a) (set-cdr! a (add1 (cdr a)))))
			       (else
				(set! specialization-statistics
				  (cons (cons op 1) 
					specialization-statistics))))))
		     (when (and specialize (not op) (procedure-type? ptype))
		       (set-car! (node-parameters node) #t)
		       (set! safe-calls (add1 safe-calls))))
		   (let ((r (if (eq? '* r) r (map (cut resolve <> typeenv) r))))
		     (d  "  result-types: ~a" r)
		     (values r op))))))))

    (define tag
      (let ((n 0))
	(lambda () 
	  (set! n (add1 n))
	  n)))

    (define (add-to-blist var flow type)
      (let loop ((var var))
	(set! blist (alist-update! (cons var flow) type blist equal?))
	(let ((a (assq var aliased)))
	  (when a
	    (d "  applying to alias: ~a -> ~a" (cdr a) type)
	    (loop (cdr a))))))

    (define (initial-argument-types dest vars argc)
      (if (and dest strict
	       (variable-mark dest '##compiler#type-source))
	  (let* ((ptype (variable-mark dest '##compiler#type))
		 (typeenv (type-typeenv ptype)))
	    (if (procedure-type? ptype)
		(map (cut resolve <> typeenv)
		     (nth-value 0 (procedure-argument-types ptype argc '() #t)))
		(make-list argc '*)))
	  (make-list argc '*)))

    (define (walk n e loc dest tail flow ctags) ; returns result specifier
      (let ((subs (node-subexpressions n))
	    (params (node-parameters n)) 
	    (class (node-class n)) )
	(dd "walk: ~a ~s (loc: ~a, dest: ~a, tail: ~a, flow: ~a)"
	    class params loc dest tail flow)
	#;(dd "walk: ~a ~s (loc: ~a, dest: ~a, tail: ~a, flow: ~a, blist: ~a, e: ~a)"
	    class params loc dest tail flow blist e)
	(set! d-depth (add1 d-depth))
	(let ((results
	       (case class
		 ((##core#the/result) (list (first params))) ; already walked
		 ((quote) (list (constant-result (first params))))
		 ((##core#undefined) '(*))
		 ((##core#proc) '(procedure))
		 ((##core#variable) (variable-result (first params) e loc flow))
		 ((##core#inline_ref)
		  (list (foreign-type->scrutiny-type (second params) 'result)))
		 ((##core#inline_loc_ref)
		  (list (foreign-type->scrutiny-type (first  params) 'result)))
		 ((if)
		  (let ((tags (cons (tag) (tag)))
			(tst (first subs))
			(nor-1 noreturn))
		    (set! noreturn #f)
		    (let* ((rt (single n "in conditional" (walk tst e loc #f #f flow tags) loc))
			   (c (second subs))
			   (a (third subs))
			   (nor0 noreturn))
		      (cond
			((and (always-true n tst rt loc) specialize)
			 (set! dropped-branches (add1 dropped-branches))
			 (mutate-node! n `(let ((,(gensym) ,tst)) ,c))
			 (walk n e loc dest tail flow ctags))
			((and (always-false n tst rt loc) specialize)
			 (set! dropped-branches (add1 dropped-branches))
			 (mutate-node! n `(let ((,(gensym) ,tst)) ,a))
			 (walk n e loc dest tail flow ctags))
			(else
			 (let* ((r1 (walk c e loc dest tail (cons (car tags) flow) #f))
				(nor1 noreturn))
			   (set! noreturn #f)
			   (let* ((r2 (walk a e loc dest tail (cons (cdr tags) flow) #f))
				 (nor2 noreturn))
			     (set! noreturn (or nor-1 nor0 (and nor1 nor2)))
			     ;; when only one branch is noreturn, add blist entries for
			     ;; all in other branch:
			     (when (or (and nor1 (not nor2))
				      (and nor2 (not nor1)))
			       (let ((yestag (if nor1 (cdr tags) (car tags))))
				(for-each
				 (lambda (ble)
				   (when (eq? (cdar ble) yestag)
				     (d "adding blist entry ~a for single returning conditional branch"
					ble)
				     (add-to-blist (caar ble) (car flow) (cdr ble))))
				 blist)))
			     (cond ((and (not (eq? '* r1)) (not (eq? '* r2)))
				   ;;(dd " branches: ~s:~s / ~s:~s" nor1 r1 nor2 r2)
				   (cond ((and (not nor1) (not nor2)
					       (not (= (length r1) (length r2))))
					  (report
					   loc
					   "branches in conditional expression differ in the number of results:~%~%~a"
					   (pp-fragment n))
					  '*)
					 (nor1 r2)
					 (nor2 r1)
					 (else
					  (dd "merge branch results: ~s + ~s" r1 r2)
					  (map (lambda (t1 t2)
						 (simplify-type `(or ,t1 ,t2)))
					       r1 r2))))
				  (else '*)))))))))
		 ((let)
		  ;; before CPS-conversion, `let'-nodes may have multiple bindings
		  (let loop ((vars params) (body subs) (e2 '()))
		    (if (null? vars)
			(walk (car body) (append e2 e) loc dest tail flow ctags)
			(let* ((var (car vars))
			       (val (car body))
			       (t (single
				   n
				   (sprintf "in `let' binding of `~a'" (real-name var))
				   (walk val e loc var #f flow #f) 
				   loc)))
			  (when (and (eq? (node-class val) '##core#variable)
				     (not (db-get db var 'assigned)))
			    (let ((var2 (first (node-parameters val))))
			      (unless (db-get db var2 'assigned) ;XXX too conservative?
				(set! aliased (alist-cons var var2 aliased)))))
			  (loop (cdr vars) (cdr body) (alist-cons (car vars) t e2))))))
		 ((##core#lambda lambda)
		  (##sys#decompose-lambda-list
		   (first params)
		   (lambda (vars argc rest)
		     (let* ((namelst (if dest (list dest) '()))
			    (inits (initial-argument-types dest vars argc))
			    (args (append inits (if rest '(#!rest) '())))
			    (e2 (append (map (lambda (v i) (cons v i))
					     (if rest (butlast vars) vars)
					     inits)
					e)))
		       (when dest 
			 (d "~a: initial-argument types: ~a" dest inits))
		       (fluid-let ((blist '())
				   (noreturn #f)
				   (aliased '()))
			 (let* ((initial-tag (tag))
				(r (walk (first subs)
					 (if rest (alist-cons rest 'list e2) e2)
					 (add-loc dest loc)
					 #f #t (list initial-tag) #f)))
			   #;(when (and specialize
				      dest
				      (variable-mark dest '##compiler#type-source)
				      (not unsafe))
			     (debugging 'x "checks argument-types" dest) ;XXX
			     ;; [1] this is subtle: we don't want argtype-checks to be 
			     ;; generated for toplevel defs other than user-declared ones. 
			     ;; But since the ##compiler#type-source mark is set AFTER
			     ;; the lambda has been walked (see below, [2]), nothing is added.
			     (generate-type-checks! n dest vars inits))
			   (list
			    (append
			     '(procedure) 
			     namelst
			     (list
			      (let loop ((argc argc) (vars vars) (args args))
				(cond ((zero? argc) args)
				      ((and (not (db-get db (car vars) 'assigned))
					    (assoc (cons (car vars) initial-tag) blist))
				       =>
				       (lambda (a)
					 (cons
					  (cond ((eq? (cdr a) '*) '*)
						(else
						 (d "adjusting procedure argument type for `~a' to: ~a"
						    (car vars) (cdr a))
						 (cdr a) ))
					  (loop (sub1 argc) (cdr vars) (cdr args)))))
				      (else 
				       (cons 
					(car args)
					(loop (sub1 argc) (cdr vars) (cdr args)))))))
			     r))))))))
		 ((set! ##core#set!)
		  (let* ((var (first params))
			 (type (variable-mark var '##compiler#type))
			 (rt (single
			      n
			      (sprintf "in assignment to `~a'" var)
			      (walk (first subs) e loc var #f flow #f)
			      loc))
			 (typeenv (append 
				   (if type (type-typeenv type) '())
				   (type-typeenv rt)))
			 (b (assq var e)) )
		    (when (and type (not b)
			       (not (or (eq? type 'deprecated)
                                        (and (pair? type)
                                             (eq? (car type) 'deprecated))))
			       (not (match-types type rt typeenv)))
		      ((if strict report-error report)
		       loc
		       "assignment of value of type `~a' to toplevel variable `~a' \
			does not match declared type `~a'"
		       rt var type))
		    (when (and (not type) ;XXX global declaration could allow this
			       (not b)
			       (not (eq? '* rt))
			       (not (db-get db var 'unknown)))
		      (and-let* ((val (or (db-get db var 'value)
					  (db-get db var 'local-value))))
			(when (and (eq? val (first subs))
				   (or (not (variable-visible? var block-compilation))
				       (not (eq? (variable-mark var '##compiler#inline) 
						 'no))))
			  (let ((rtlst (list (cons #f (tree-copy rt)))))
			    (smash-component-types! rtlst "global")
			    (let ((rt (cdar rtlst)))
			      (debugging '|I| (sprintf "(: ~s ~s)" var rt))
			      ;; [2] sets property, but lambda has already been walked,
			      ;; so no type-checks are generated (see also [1], above)
			      ;; note that implicit declarations are not enforcing
			      (mark-variable var '##compiler#type-source 'inference)
			      (mark-variable var '##compiler#type rt))))))
		    (when b
		      (cond ((eq? 'undefined (cdr b)) (set-cdr! b rt))
			    #;(strict
			     (let ((ot (or (blist-type var flow) (cdr b))))
			       ;;XXX compiler-syntax for "map" will introduce
			       ;;    assignments that trigger this warning, so this
			       ;;    is currently disabled
			       (unless (compatible-types? ot rt)
				 (report
				  loc
				  "variable `~a' of type `~a' was modified to a value of type `~a'"
				  var ot rt)))))
		      ;; don't use "add-to-blist" since the current operation does not affect aliases
		      (let ((t (if (or strict (not (db-get db var 'captured)))
				   rt 
				   '*))
			    (fl (car flow)))
			(let loop ((bl blist) (f #f))
			  (cond ((null? bl)
				 (unless f
				   (set! blist (alist-cons (cons var fl) t blist))))
				((eq? (caaar bl) var)
				 (let ((t (simplify-type `(or ,t ,(cdar bl)))))
				   (dd "assignment modifies blist entry ~s -> ~a"
				       (caar bl) t)
				   (set-cdr! (car bl) t)
				   (loop (cdr bl) (eq? fl (cdaar bl)))))
				(else (loop (cdr bl) f))))))

		    (when (always-immediate var rt loc)
		      (set! assigned-immediates (add1 assigned-immediates))
		      (set-cdr! params '(#t)))

		    '(undefined)))
		 ((##core#primitive) '*)
		 ((##core#call)
		  (let* ((f (fragment n))
			 (len (length subs))
			 (args (map (lambda (n i)
				      (make-node
				       '##core#the/result
				       (list
					(single
					 n
					 (sprintf 
					     "in ~a of procedure call `~s'"
					   (if (zero? i)
					       "operator position"
					       (sprintf "argument #~a" i))
					   f)
					 (walk n e loc #f #f flow #f) 
					 loc))
				       (list n)))
				    subs 
				    (iota len)))
			 (fn (walked-result (car args)))
			 (pn (procedure-name fn))
			 (typeenv (type-typeenv
				   `(or ,@(map walked-result args)))) ; hack
			 (enforces
			  (and pn (variable-mark pn '##compiler#enforce)))
			 (pt (and pn (variable-mark pn '##compiler#predicate))))
		    (let-values (((r specialized?) 
				  (call-result n args e loc params typeenv)))
		      (define (smash)
			(when (and (not strict)
				   (or (not pn)
				       (and
					(not (variable-mark pn '##compiler#pure))
					(not (variable-mark pn '##compiler#clean)))))
			  (smash-component-types! e "env")
			  (smash-component-types! blist "blist")))
		      (cond (specialized?
			     (walk n e loc dest tail flow ctags)
			     (smash)
			     ;; keep type, as the specialization may contain icky stuff
			     ;; like "##core#inline", etc.
			     (if (eq? '* r)
				 r
				 (map (cut resolve <> typeenv) r)))
			    (else
			     (for-each
			      (lambda (arg argr)
				(when (eq? '##core#variable (node-class arg))
				  (let* ((var (first (node-parameters arg)))
					 (a (or (blist-type var flow) (alist-ref var e)))
					 (argr (resolve argr typeenv))
					 (oparg? (eq? arg (first subs)))
					 (pred (and pt
						    ctags
						    (not (db-get db var 'assigned)) 
						    (not oparg?))))
				    (cond (pred
					   ;;XXX is this needed? "typeenv" is the te of "args",
					   ;;    not of "pt":
					   (let ((pt (resolve pt typeenv)))
					     (d "  predicate `~a' indicates `~a' is ~a in flow ~a"
						pn var pt (car ctags))
					     (add-to-blist
					      var (car ctags)
					      (if (not a) pt (refine-types a pt)))
					     ;; if the variable type is an "or"-type, we can
					     ;; can remove all elements that match the predicate
					     ;; type
					     (when a
					       ;;XXX hack, again:
					       (let ((at (refine-types a `(not ,pt))))
						 (when at
						   (d "  predicate `~a' indicates `~a' is ~a in flow ~a"
						      pn var at (cdr ctags))
						   (add-to-blist var (cdr ctags) at))))))
					  (a
					   (when enforces
					     (let ((ar (if (db-get db var 'assigned)
							   '* ; XXX necessary?
							   (refine-types a argr))))
					       (d "  assuming: ~a -> ~a (flow: ~a)" 
						  var ar (car flow))
					       (add-to-blist var (car flow) ar)
					       (when ctags
						 (add-to-blist var (car ctags) ar)
						 (add-to-blist var (cdr ctags) ar)))))
					  ((and oparg?
						(variable-mark 
						 var
						 '##compiler#special-result-type))
					   => (lambda (srt)
						(dd "  hardcoded special result-type: ~a" var)
						(set! r (srt n args loc r))))))))
			      subs
			      (cons 
			       fn
			       (nth-value 
				0 
				(procedure-argument-types fn (sub1 len) typeenv))))
			     (smash)
			     (if (eq? '* r)
				 r
				 (map (cut resolve <> typeenv) r)))))))
		 ((##core#the)
		  (let ((t (first params))
			(rt (walk (first subs) e loc dest tail flow ctags)))
		    (cond ((eq? rt '*))
			  ((null? rt)
			   (report
			    loc
			    "expression returns zero values but is declared to have \
			     a single result of type `~a'" t))
			  (else
			   (when (> (length rt) 1)
			     (report
			      loc
			      "expression returns ~a values but is declared to have \
			       a single result" (length rt)))
			   (when (and (second params)
				      (not (compatible-types? t (first rt))))
			     ((if strict report-error report-notice)
			      loc
			      "expression returns a result of type `~a' but is \
			       declared to return `~a', which is not compatible"
			      (first rt) t))))
		    (list t)))
		 ((##core#typecase)
		  (let* ((ts (walk (first subs) e loc #f #f flow ctags))
			 (trail0 trail)
			 (typeenv0 (type-typeenv (car ts))))
		    ;; first exp is always a variable so ts must be of length 1
		    (let loop ((types (cdr params)) (subs (cdr subs)))
		      (if (null? types)
			  (quit-compiling
			   "~a~ano clause applies in `compiler-typecase' for expression of type `~a':~a"
			   (location-name loc)
			   (node-source-prefix n)
			   (type-name (car ts))
			   (string-intersperse
			    (map (lambda (t) (sprintf "\n    ~a" (type-name t)))
				 (cdr params)) ""))
			  (let ((typeenv (append (type-typeenv (car types)) typeenv0)))
			    (if (match-types (car types) (car ts) typeenv #t)
				(begin ; drops exp
				  (mutate-node! n (car subs))
				  (walk n e loc dest tail flow ctags))
				(begin
				  (trail-restore trail0 typeenv)
				  (loop (cdr types) (cdr subs)))))))))
		 ((##core#switch ##core#cond)
		  (bomb "scrutinize: unexpected node class" class))
		 (else
		  (for-each (lambda (n) (walk n e loc #f #f flow #f)) subs)
		  '*))))
	  (set! d-depth (sub1 d-depth))
	  (dd "  ~a -> ~a" class results)
	  results)))

    (let ((rn (walk (first (node-subexpressions node)) '() '() #f #f (list (tag)) #f)))
      (when (pair? specialization-statistics)
	(with-debugging-output
	 '(o e)
	 (lambda ()
	   (print "specializations:")
	   (for-each 
	    (lambda (ss)
	      (printf "  ~a ~s~%" (cdr ss) (car ss)))
	    specialization-statistics))))
      (when (positive? safe-calls)
	(debugging '(o e) "safe calls" safe-calls))
      (when (positive? dropped-branches)
	(debugging '(o e) "dropped branches" dropped-branches))
      (when (positive? assigned-immediates)
	(debugging '(o e) "assignments to immediate values" assigned-immediates))
      (when errors
	(quit-compiling "some variable types do not satisfy strictness"))
      rn)))
      

;;; replace pair/vector types with components to variants with undetermined
;;  component types (used for env or blist); also convert "list[-of]" types
;;  into "pair", since mutation may take place

(define (smash-component-types! lst where)
  ;; assumes list of the form "((_ . T1) ...)"
  (do ((lst lst (cdr lst)))
      ((null? lst))
    (let loop ((t (cdar lst))
	       (change! (cute set-cdr! (car lst) <>)))
      (when (pair? t)
	(case (car t)
	  ((vector-of)
	   (dd "  smashing `~s' in ~a" (caar lst) where)
	   (change! 'vector)
	   (car t))
	  ((vector)
	   (dd "  smashing `~s' in ~a" (caar lst) where)
	   ;; (vector x y z) => (vector * * *)
	   (change! (cons 'vector (map (constantly '*) (cdr t))))
	   (car t))
	  ((list-of list)
	   (dd "  smashing `~s' in ~a" (caar lst) where)
	   (change! '(or pair null))
	   (car t))
	  ((pair)
	   (dd "  smashing `~s' in ~a" (caar lst) where)
	   (change! (car t))
	   (car t))
	  ((forall)
	   (loop (third t)
		 (cute set-car! (cddr t) <>))))))))


;;; Type-matching
;
; - "all" means: all elements in `or'-types in second argument must match

(define (match-types t1 t2 #!optional (typeenv (type-typeenv `(or ,t1 ,t2))) all)

  (define (match-args args1 args2)
    (d "match args: ~s <-> ~s" args1 args2)
    (let loop ((args1 args1) (args2 args2) (opt1 #f) (opt2 #f))
      (cond ((null? args1) 
	     (or opt2
		 (null? args2)
		 (optargs? (car args2))))
	    ((null? args2) 
	     (or opt1
		 (optargs? (car args1))))
	    ((eq? '#!optional (car args1))
	     (loop (cdr args1) args2 #t opt2))
	    ((eq? '#!optional (car args2))
	     (loop args1 (cdr args2) opt1 #t))
	    ((eq? '#!rest (car args1))
	     (match-rest (rest-type (cdr args1)) args2 opt2))
	    ((eq? '#!rest (car args2))
	     (match-rest (rest-type (cdr args2)) args1 opt1))
	    ((match1 (car args1) (car args2))
	     (loop (cdr args1) (cdr args2) opt1 opt2))
	    (else #f))))

  (define (match-rest rtype args opt)	;XXX currently ignores `opt'
    (let-values (((head tail) (span (lambda (x) (not (eq? '#!rest x))) args)))
      (and (every			
	    (lambda (t)
	      (or (eq? '#!optional t)
		  (match1 rtype t)))
	    head)
	   (match1 rtype (if (pair? tail) (rest-type (cdr tail)) '*)))))

  (define (optargs? a)
    (memq a '(#!rest #!optional)))

  (define (match-results results1 results2)
    (cond ((eq? '* results1))
	  ((eq? '* results2) (not all))
	  ((null? results1) (null? results2))
	  ((null? results2) #f)
	  ((and (memq (car results1) '(undefined noreturn))
		(memq (car results2) '(undefined noreturn))))
	  ((match1 (car results1) (car results2)) 
	   (match-results (cdr results1) (cdr results2)))
	  (else #f)))

  (define (rawmatch1 t1 t2)
    (fluid-let ((all #f))
      (match1 t1 t2)))

  (define (every-match1 lst1 lst2)
    (let loop ((lst1 lst1) (lst2 lst2))
      (cond ((null? lst1))
	    ((match1 (car lst1) (car lst2)) (loop (cdr lst1) (cdr lst2)))
	    (else #f))))

  (define (match1 t1 t2)
    ;; note: the order of determining the type is important
    (dd "   match1: ~s <-> ~s" t1 t2)
    (cond ((eq? t1 t2))
	  ;;XXX do we have to handle circularities?
	  ((and (symbol? t1) (assq t1 typeenv)) => 
	   (lambda (e) 
	     (cond ((second e)
		    (and (match1 (second e) t2)
			 (or (not (third e)) ; constraint
			     (rawmatch1 (third e) t2))))
		   ;; special case for two unbound typevars
		   ((and (symbol? t2) (assq t2 typeenv)) =>
		    (lambda (e2)
		      ;;XXX probably not fully right, consider:
		      ;;    (forall (a b) ((a a b) ->)) + (forall (c d) ((c d d) ->))
		      ;;    or is this not a problem? I don't know right now...
		      (or (not (second e2))
			  (and (match1 t1 (second e2))
			       (or (not (third e2)) ; constraint
				   (rawmatch1 t1 (third e2)))))))
		   ((or (not (third e))
			(rawmatch1 (third e) t2))
		    (dd "   unify ~a = ~a" t1 t2)
		    (set! trail (cons t1 trail))
		    (set-car! (cdr e) t2)
		    #t)
		   (else #f))))
	  ((and (symbol? t2) (assq t2 typeenv)) => 
	   (lambda (e) 
	     (cond ((second e)
		    (and (match1 t1 (second e))
			 (or (not (third e)) ; constraint
			     (rawmatch1 t1 (third e)))))
		   ((or (not (third e))
			(rawmatch1 t1 (third e)))
		    (dd "   unify ~a = ~a" t2 t1)
		    (set! trail (cons t2 trail))
		    (set-car! (cdr e) t1)
		    #t)
		   (else #f))))
	  ((eq? t1 '*))
	  ((eq? t2 '*) (not all))
	  ((eq? t1 'undefined) #f)
	  ((eq? t2 'undefined) #f)
	  ((eq? t1 'noreturn))
	  ((eq? t2 'noreturn))
	  ((maybe-expand-type t1) => (cut match1 <> t2))
	  ((maybe-expand-type t2) => (cut match1 t1 <>))
	  ((and (pair? t1) (eq? 'not (car t1)))
	   (fluid-let ((all (not all)))
	     (let* ((trail0 trail)
		    (m (match1 (cadr t1) t2)))
	       (trail-restore trail0 typeenv)
	       (not m))))
	  ((and (pair? t2) (eq? 'not (car t2)))
	   (and (not all)
		(fluid-let ((all #t))
		  (let* ((trail0 trail)
			 (m (match1 (cadr t2) t1)))
		    (trail-restore trail0 typeenv)
		    (not m)))))
	  ;; this is subtle: "or" types for t2 are less restrictive,
	  ;; so we handle them before "or" types for t1
	  ((and (pair? t2) (eq? 'or (car t2)))
	   (over-all-instantiations
	    (cdr t2)
	    typeenv
	    all
	    (lambda (t) (match1 t1 t))))
	  ;; s.a.
	  ((and (pair? t1) (eq? 'or (car t1))) 
	   (over-all-instantiations
	    (cdr t1)
	    typeenv
	    #f
	    (lambda (t) (match1 t t2)))) ; o-a-i ensures at least one element matches
	  ((and (pair? t1) (eq? 'forall (car t1)))
	   (match1 (third t1) t2)) ; assumes typeenv has already been extracted
	  ((and (pair? t2) (eq? 'forall (car t2)))
	   (match1 t1 (third t2))) ; assumes typeenv has already been extracted
	  ((eq? 'procedure t1)
	   (and (pair? t2) (eq? 'procedure (car t2))))
	  ((eq? 'procedure t2)
	   (and (not all)
		(pair? t1) (eq? 'procedure (car t1))))
	  ((eq? t1 'null)
	   (and (not all)
		(pair? t2) (eq? 'list-of (car t2))))
	  ((eq? t2 'null)
	   (and (pair? t1) (eq? 'list-of (car t1))))
	  ((and (pair? t1) (pair? t2) (eq? (car t1) (car t2)))
	   (case (car t1)
	     ((procedure)
	      (let ((args1 (procedure-arguments t1))
		    (args2 (procedure-arguments t2))
		    (results1 (procedure-results t1))
		    (results2 (procedure-results t2)))
		(and (match-args args1 args2)
		     (match-results results1 results2))))
	     ((struct) (equal? t1 t2))
	     ((pair) (every-match1 (cdr t1) (cdr t2)))
	     ((list-of vector-of) (match1 (second t1) (second t2)))
	     ((list vector)
	      (and (= (length t1) (length t2))
		   (every-match1 (cdr t1) (cdr t2))))
	     ((refine)
	      (and (match1 (third t1) (third t2))
		   (or (not all)
		       (lset<=/eq? (second t1) (second t2)))))
	     (else #f)))
	  ((and (pair? t1) (eq? 'refine (car t1)))
	   (and (not all) (match1 (third t1) t2)))
	  ((and (pair? t2) (eq? 'refine (car t2)))
	   (match1 t1 (third t2)))
	  ((and (pair? t1) (eq? 'pair (car t1)))
	   (and (pair? t2)
		(case (car t2)
		  ((list-of)
		   (and (not all)
			(match1 (second t1) (second t2))
			(match1 (third t1) t2)))
		  ((list)
		   (and (pair? (cdr t2))
			(match1 (second t1) (second t2))
			(match1 (third t1)
				(if (null? (cddr t2))
				    'null
				    `(list ,@(cddr t2))))))
		  (else #f))))
	  ((and (pair? t2) (eq? 'pair (car t2)))
	   (and (pair? t1)
		(case (car t1)
		  ((list-of)
		   (and (not all)
			(match1 (second t1) (second t2))
			(match1 t1 (third t2))))
		  ((list)
		   (and (pair? (cdr t1))
			(match1 (second t1) (second t2))
			(match1 (if (null? (cddr t1))
				    'null
				    `(list ,@(cddr t1)))
				(third t2))))
		  (else #f))))
	  ((and (pair? t1) (eq? 'list (car t1)))
	   (and (not all)
		(pair? t2) (eq? 'list-of (car t2))
		(over-all-instantiations
		 (cdr t1)
		 typeenv
		 #t
		 (cute match1 <> (second t2)))))
	  ((and (pair? t1) (eq? 'list-of (car t1)))
	   (and (pair? t2) (eq? 'list (car t2))
		(over-all-instantiations
		 (cdr t2)
		 typeenv
		 #t
		 (cute match1 (second t1) <>))))
	  ((and (pair? t1) (eq? 'vector (car t1)))
	   (and (not all)
		(pair? t2) (eq? 'vector-of (car t2))
		(over-all-instantiations
		 (cdr t1)
		 typeenv
		 #t
		 (cute match1 <> (second t2)))))
	  ((and (pair? t1) (eq? 'vector-of (car t1)))
	   (and (pair? t2) (eq? 'vector (car t2))
		(over-all-instantiations
		 (cdr t2)
		 typeenv
		 #t
		 (cute match1 (second t1) <>))))
	  (else #f)))

  (dd "match (~a) ~a <-> ~a" (if all "all" "any") t1 t2)
  (let ((m (match1 t1 t2)))
    (dd "match (~a) ~s <-> ~s -> ~s" (if all "all" "any") t1 t2 m)
    m))


(define (match-argument-types typelist atypes typeenv)
  ;; this doesn't need optional: it is only used for predicate- and specialization
  ;; matching
  (let loop ((tl typelist) (atypes atypes))
    (cond ((null? tl) (null? atypes))
	  ((null? atypes) #f)
	  ((equal? '(#!rest) tl))
	  ((eq? (car tl) '#!rest)
	   (every 
	    (lambda (at)
	      (match-types (cadr tl) at typeenv #t))
	    atypes))
	  ((match-types (car tl) (car atypes) typeenv #t)
	   (loop (cdr tl) (cdr atypes)))
	  (else #f))))


;;; Simplify type specifier
;
; - coalesces "forall" and renames type-variables
; - also removes unused typevars

(define (simplify-type t)
  (let ((typeenv '())			; ((VAR1 . NEWVAR1) ...)
	(constraints '())		; ((VAR1 TYPE1) ...)
	(used '()))
    (define (subst x)
      (cond ((symbol? x)
	     (cond ((assq x typeenv) => cdr)
		   (else x)))
	    ((pair? x)
	     (cons (subst (car x)) (subst (cdr x))))
	    (else x)))
    (define (simplify t)
      ;;(dd "simplify/rec: ~s" t)
      (call/cc 
       (lambda (return)
	 (cond ((pair? t)
		(case (car t)
		  ((forall)
		   (let ((typevars (second t)))
		     (set! typeenv
		       (append (map (lambda (v)
				      (let ((v (if (symbol? v) v (first v))))
					(let ((v* (gensym v)))
					  (mark-variable v* '##core#real-name v)
					  (cons v v*))))
				    typevars)
			       typeenv))
		     (set! constraints 
		       (append (filter-map 
				(lambda (v)
				  (and (pair? v) v))
				typevars)
			       constraints))
		     (simplify (third t))))
		  ((or)
		   (let ((ts (delete-duplicates (map simplify (cdr t)) eq?)))
		     (cond ((null? ts) '*)
			   ((null? (cdr ts)) (car ts))
			   ((> (length ts) +maximal-union-type-length+)
			    (d "union-type cutoff! (~a): ~s" (length ts) ts)
			    '*)
			   ((every procedure-type? ts)
			    (if (any (cut eq? 'procedure <>) ts)
				'procedure
				(foldl
				 (lambda (pt t)
				   (let* ((name1 (procedure-name t))
					  (atypes1 (procedure-arguments t))
					  (rtypes1 (procedure-results t))
					  (name2 (procedure-name pt))
					  (atypes2 (procedure-arguments pt))
					  (rtypes2 (procedure-results pt)))
				     (append
				      '(procedure)
				      (if (and name1 name2 (eq? name1 name2)) (list name1) '())
				      (list (merge-argument-types atypes1 atypes2))
				      (merge-result-types rtypes1 rtypes2))))
				 (car ts)
				 (cdr ts))))
			   ((lset=/eq? '(true false) ts) 'boolean)
			   ((lset=/eq? '(fixnum bignum) ts) 'integer)
			   ((lset=/eq? '(fixnum float bignum ratnum cplxnum) ts) 'number)
			   (else
			    (let* ((ts (append-map
					(lambda (t)
					  (let ((t (simplify t)))
					    (cond ((and (pair? t) (eq? 'or (car t)))
						   (cdr t))
						  ((eq? t 'undefined) (return 'undefined))
						  ((eq? t 'noreturn) (return '*))
						  (else (list t)))))
					ts))
				   (ts2 (let loop ((ts ts) (done '()))
					  (cond ((null? ts) (reverse done))
						((eq? '* (car ts)) (return '*))
						((any (cut type<=? (car ts) <>) (cdr ts))
						 (loop (cdr ts) done))
						((any (cut type<=? (car ts) <>) done)
						 (loop (cdr ts) done))
						(else (loop (cdr ts) (cons (car ts) done)))))))
				  (if (equal? ts2 (cdr t))
				      t
				      (simplify `(or ,@ts2))))))))
		  ((refine)
		   (let ((rs (second t))
			 (t2 (simplify (third t))))
		     (cond ((null? rs) t2)
		           ((refinement-type? t2)
			    (list 'refine (lset-union/eq? (second t2) rs) (third t2)))
			   (else
			    (list 'refine (delete-duplicates rs eq?) t2)))))
		  ((pair)
		   (let ((tcar (simplify (second t)))
			 (tcdr (simplify (third t))))
		     (if (and (eq? '* tcar) (eq? '* tcdr))
			 'pair
			 (canonicalize-list-type
			  `(pair ,tcar ,tcdr)))))
		  ((vector-of)
		   (let ((t2 (simplify (second t))))
		     (if (eq? t2 '*)
			 'vector
			 `(,(car t) ,t2))))
		  ((list-of)
		   (let ((t2 (simplify (second t))))
		     (if (eq? t2 '*)
			 'list
			 `(,(car t) ,t2))))
		  ((list)
		   (if (null? (cdr t))
		       'null
		       `(list ,@(map simplify (cdr t)))))
		  ((vector)
		   `(vector ,@(map simplify (cdr t))))
		  ((procedure)
		   (let* ((name (and (named? t) (cadr t)))
			  (rtypes (if name (cdddr t) (cddr t))))
		     (append
		      '(procedure)
		      (if name (list name) '())
		      (list (map simplify (if name (third t) (second t))))
		      (if (eq? '* rtypes)
			  '*
			  (map simplify rtypes)))))
		  (else t)))
	       ((assq t typeenv) =>
		(lambda (e)
		  (set! used (lset-adjoin/eq? used t))
		  (cdr e)))
	       (else t)))))
    (let ((t2 (simplify t)))
      (when (pair? used)
	(set! t2 
	  `(forall ,(filter-map
		     (lambda (e)
		       (and (memq (car e) used)
			    (let ((v (cdr e)))
			      (cond ((assq (car e) constraints) =>
				     (lambda (c)
				       (list v (simplify (cadr c)))))
				    (else v)))))
		     typeenv)
		   ,(subst t2))))
      (dd "simplify: ~a -> ~a" t t2)
      t2)))

(define (maybe-expand-type t)
  (and (symbol? t)
       (alist-ref t type-expansions eq?)))

;;; Merging types

(define (merge-argument-types ts1 ts2) 
  ;; this could be more elegantly done by combining non-matching arguments/llists
  ;; into "(or (procedure ...) (procedure ...))" and then simplifying
  (cond ((null? ts1) 
	 (cond ((null? ts2) '())
	       ((memq (car ts2) '(#!rest #!optional)) ts2)
	       (else '(#!rest))))
	((null? ts2) '(#!rest))		;XXX giving up
	((eq? '#!rest (car ts1))
	 (cond ((and (pair? ts2) (eq? '#!rest (car ts2)))
		`(#!rest
		  ,(simplify-type
		    `(or ,(rest-type (cdr ts1))
			 ,(rest-type (cdr ts2))))))
	       (else '(#!rest))))	;XXX giving up
	((eq? '#!optional (car ts1))
	 (cond ((and (pair? ts2) (eq? '#!optional (car ts2)))
		`(#!optional 
		  ,(simplify-type `(or ,(cadr ts1) ,(cadr ts2)))
		  ,@(merge-argument-types (cddr ts1) (cddr ts2))))
	       (else '(#!rest))))	;XXX
	((memq (car ts2) '(#!rest #!optional))
	 (merge-argument-types ts2 ts1))
	(else (cons (simplify-type `(or ,(car ts1) ,(car ts2)))
		    (merge-argument-types (cdr ts1) (cdr ts2))))))

(define (merge-result-types ts11 ts21) ;XXX possibly overly conservative
  (call/cc
   (lambda (return)
     (let loop ((ts1 ts11) (ts2 ts21))
       (cond ((and (null? ts1) (null? ts2)) '())
	     ((or (atom? ts1) (atom? ts2)) (return '*))
	     (else (cons (simplify-type `(or ,(car ts1) ,(car ts2)))
			 (loop (cdr ts1) (cdr ts2)))))))))


(define (compatible-types? t1 t2 #!optional (te (type-typeenv `(or ,t1 ,t2))))
  (or (type<=? t1 t2 te)
      (type<=? t2 t1 te)))

(define (type-min t1 t2 #!optional (te (type-typeenv `(or ,t1 ,t2))))
  (cond ((type<=? t1 t2 te) t1)
	((type<=? t2 t1 te) t2)
	(else #f)))

(define (type<=? t1 t2 #!optional (te (type-typeenv `(or ,t1 ,t2))))
  (with-trail-restore
   te
   (lambda ()
     (match-types t2 t1 te #t))))

;;
;; Combines the information in `t1' and `t2' to produce a smaller type,
;; with a preference for `t2' if no smaller type can be determined.
;; Merges refinements at each step.
;;

(define (refine-types t1 t2)

  (define (refine t1 t2 te)
    (let loop ((t1 t1) (t2 t2))
      (cond
       ((maybe-expand-type t1) => (cut loop <> t2))
       ((maybe-expand-type t2) => (cut loop t1 <>))
	((and (pair? t1) (memq (car t1) '(forall refine)))
	 (let ((t1* (loop (third t1) t2)))
	   (and t1* (list (car t1) (second t1) t1*))))
	((and (pair? t2) (memq (car t2) '(forall refine)))
	 (let ((t2* (loop t1 (third t2))))
	   (and t2* (list (car t2) (second t2) t2*))))
	;; (or pair null) ~> (list-of a) -> (list-of a)
	((and (pair? t1) (eq? (car t1) 'or)
	      (lset=/eq? '(null pair) (cdr t1))
	      (and (pair? t2) (eq? 'list-of (car t2))))
	 t2)
	((and (pair? t1) (eq? (car t1) 'or))
	 (let ((ts (filter-map (lambda (t) (loop t t2)) (cdr t1))))
	   (and (pair? ts) (cons 'or ts))))
	((and (pair? t1)
	      (memq (car t1) '(pair list vector vector-of list-of))
	      (pair? t2)
	      (eq? (car t1) (car t2))
	      (eq? (length t1) (length t2)))
	 (let ((ts (map loop (cdr t1) (cdr t2))))
	   (and (every identity ts) (cons (car t1) ts))))
	(else
	 (type-min t1 t2 te)))))

  (let* ((te (type-typeenv `(or ,t1 ,t2)))
	 (rt (or (refine t1 t2 te) t2)))
    (if (eq? rt t2)
	rt
	(simplify-type rt))))

;;; various operations on procedure types

(define (procedure-type? t)
  (or (eq? 'procedure t)
      (and (pair? t) 
	   (case (car t)
	     ((forall) (procedure-type? (third t)))
	     ((procedure) #t)
	     ((or) (every procedure-type? (cdr t)))
	     (else #f)))))

(define (procedure-name t)
  (and (pair? t)
       (case (car t)
	 ((forall) (procedure-name (third t)))
	 ((procedure)
	  (let ((n (cadr t)))
	    (cond ((string? n) (string->symbol n))
		  ((symbol? n) n)
		  (else #f))))
	 (else #f))))

(define (procedure-arguments t)
  (and (pair? t)
       (case (car t)
	 ((forall) (procedure-arguments (third t)))
	 ((procedure)
	  (let ((n (second t)))
	    (if (or (string? n) (symbol? n))
		(third t)
		(second t))))
	 (else (bomb "procedure-arguments: not a procedure type" t)))))

(define (procedure-results t)
  (and (pair? t)
       (case (car t)
	 ((forall) (procedure-results (third t)))
	 ((procedure)
	  (let ((n (second t)))
	    (if (or (string? n) (symbol? n))
		(cdddr t)
		(cddr t))))
	 (else (bomb "procedure-results: not a procedure type" t)))))

(define (procedure-argument-types t n typeenv #!optional norest)
  (let loop1 ((t t) (done '()))
    (cond ((and (pair? t)
		(eq? 'procedure (car t)))
	   (let* ((vf #f)
		  (ok #t)
		  (alen 0)
		  (llist
		   ;; quite a mess
		   (let loop ((at (if (or (string? (second t)) (symbol? (second t)))
				      (third t)
				      (second t)))
			      (m n)
			      (opt #f))
		     (cond ((null? at)
			    (set! ok (or opt (zero? m)))
			    '())
			   ((eq? '#!optional (car at))
			    (if norest
				'()
				(loop (cdr at) m #t) ))
			   ((eq? '#!rest (car at))
			    (cond (norest '())
				  (else
				   (set! vf (and (pair? (cdr at)) (eq? 'values (cadr at))))
				   (make-list m (rest-type (cdr at))))))
			   ((and opt (<= m 0)) '())
			   (else
			    (set! ok (positive? m))
			    (set! alen (add1 alen))
			    (cons (car at) (loop (cdr at) (sub1 m) opt)))))))
	     (values llist vf ok alen)))
	  ((and (pair? t) (eq? 'forall (car t)))
	   (loop1 (third t) done)) ; assumes typeenv has already been extracted
	  ((assq t typeenv) =>
	   (lambda (e)
	     (let ((t2 (second e)))
	       (if (and t2 (memq t2 done))
		   (loop1 '* done)		; circularity
		   (loop1 t2 (cons t done))))))
	  (else (values (make-list n '*) #f #t n)))))

(define (procedure-result-types t values-rest? args typeenv)
  (define (loop1 t)
    (cond (values-rest? args)
	  ((assq t typeenv) => (lambda (e) (loop1 (second e))))
	  ((and (pair? t) (eq? 'procedure (car t)))
	   (call/cc
	    (lambda (return)
	      (let loop ((rt (if (or (string? (second t)) (symbol? (second t)))
				 (cdddr t)
				 (cddr t))))
		(cond ((null? rt) '())
		      ((memq rt '(* noreturn)) (return '*))
		      (else (cons (car rt) (loop (cdr rt)))))))))
	  ((and (pair? t) (eq? 'forall (car t)))
	   (loop1 (third t))) ; assumes typeenv has already been extracted
	  (else '*)))
  (loop1 t))

(define (named? t)
  (and (pair? t) 
       (case (car t)
	 ((procedure)
	  (not (or (null? (cadr t)) (pair? (cadr t)))))
	 ((forall) (named? (third t)))
	 (else #f))))

(define (rest-type r)
  (cond ((null? r) '*)
	((eq? 'values (car r)) '*)
	(else (car r))))

(define (noreturn-procedure-type? ptype)
  (and (pair? ptype)
       (case (car ptype)
	 ((procedure)
	  (and (list? ptype)
	       (equal? '(noreturn)
		       (if (pair? (second ptype))
			   (cddr ptype)
			   (cdddr ptype)))))
	 ((forall)
	  (noreturn-procedure-type? (third ptype)))
	 (else #f))))

(define (noreturn-type? t)
  (or (eq? 'noreturn t)
      (and (pair? t)
	   (case (car t)
	     ((or) (any noreturn-type? (cdr t)))
	     ((forall) (noreturn-type? (third t)))
	     (else #f)))))

;;; Refinement type helpers

(define (refinement-type? t)
  (and (pair? t)
       (case (first t)
	 ((refine) #t)
	 ((forall) (refinement-type? (third t)))
	 (else #f))))

;;; Type-environments and -variables

(define (type-typeenv t)
  (let ((te '()))
    (let loop ((t t))
      (when (pair? t)
	(case (car t)
	  ((refine)
	   (loop (third t)))
	  ((procedure)
	   (cond ((or (string? (second t)) (symbol? (second t)))
		  (for-each loop (third t))
		  (when (pair? (cdddr t))
		    (for-each loop (cdddr t))))
		 (else
		  (for-each loop (second t))
		  (when (pair? (cddr t))
		    (for-each loop (cddr t))))))
	  ((forall)
	   (set! te (append (map (lambda (tv) 
				   (if (symbol? tv)
				       (list tv #f #f)
				       (list (first tv) #f (second tv))))
				 (second t))
			    te))
	   (loop (third t)))
	  ((or)
	   (for-each loop (cdr t))))))
    te))

(define (trail-restore tr typeenv)
  (do ((tr2 trail (cdr tr2)))
      ((eq? tr2 tr))
    (let ((a (assq (car tr2) typeenv)))
      (set-car! (cdr a) #f))))

(define (with-trail-restore typeenv thunk)
  (let* ((trail0 trail)
	 (result (thunk)))
    (trail-restore trail0 typeenv)
    result))

(define (resolve t typeenv)
  (simplify-type			;XXX do only when necessary
   (let resolve ((t t) (done '()))
     (cond ((assq t typeenv) => 
	    (lambda (a)
	      (let ((t2 (second a)))
		(if (or (not t2)
			(memq t2 done))	; circular reference
		    (if (third a)
			(resolve (third a) (cons t done))
			'*)
		    (resolve t2 (cons t done))))))
	   ((not (pair? t)) 
	    (if (or (memq t value-types) (memq t basic-types))
		t
		(bomb "resolve: can't resolve unknown type-variable" t)))
	   (else 
	    (case (car t)
	      ((or) `(or ,@(map (cut resolve <> done) (cdr t))))
	      ((not) `(not ,(resolve (second t) done)))
	      ((forall refine)
	       (list (car t) (second t) (resolve (third t) done)))
	      ((pair list vector vector-of list-of)
	       (cons (car t) (map (cut resolve <> done) (cdr t))))
	      ((procedure)
	       (let* ((name (procedure-name t))
		      (argtypes (procedure-arguments t))
		      (rtypes (procedure-results t)))
		 `(procedure
		   ,@(if name (list name) '())
		   ,(let loop ((args argtypes))
		      (cond ((null? args) '())
			    ((eq? '#!rest (car args))
			     (if (equal? '(values) (cdr args))
				 args
				 (cons (car args) (loop (cdr args)))))
			    ((eq? '#!optional (car args))
			     (cons (car args) (loop (cdr args))))
			    (else (cons (resolve (car args) done) (loop (cdr args))))))
		   ,@(if (eq? '* rtypes)
			 '*
			 (map (cut resolve <> done) rtypes)))))
	      (else t)))))))


;;; type-db processing

(define (load-type-database name specialize #!optional
                            (path (repository-path)))
  (define (clean! name)
    (when specialize (mark-variable name '##compiler#clean #t)))
  (define (pure! name)
    (when specialize (mark-variable name '##compiler#pure #t)))
  (and-let* ((dbfile (if (not path)
			 (and (##sys#file-exists? name #t #f #f) name)
			 (chicken.load#find-file name path))))
    (debugging 'p (sprintf "loading type database `~a' ...~%" dbfile))
    (fluid-let ((scrutiny-debug #f))
      (for-each
       (lambda (e)
	 (let* ((name (car e))
		(old (variable-mark name '##compiler#type))
		(specs (and (pair? (cddr e)) (cddr e)))
		(new
		 (let adjust ((new (cadr e)))
		   (if (pair? new)
		       (cond ((and (vector? (car new))
				   (eq? 'procedure (vector-ref (car new) 0)))
			      (let loop ((props (cdr (vector->list (car new)))))
				(unless (null? props)
				  (case (car props)
				    ((#:pure)
				     (pure! name)
				     (loop (cdr props)))
				    ((#:clean)
				     (clean! name)
				     (loop (cdr props)))
				    ((#:enforce)
				     (mark-variable name '##compiler#enforce #t)
				     (loop (cdr props)))
				    ((#:foldable)
				     (mark-variable name '##compiler#foldable #t)
				     (loop (cdr props)))
				    ((#:predicate)
				     (mark-variable name '##compiler#predicate (cadr props))
				     (loop (cddr props)))
				    (else
				     (bomb
				      "load-type-database: invalid procedure-type property"
				      (car props) new)))))
			      `(procedure ,@(cdr new)))
			     ((eq? 'forall (car new))
			      `(forall ,(second new) ,(adjust (third new))))
			     (else new))
		       new))))
	   ;; validation is needed, even though .types-files can be considered
	   ;; correct, because type variables have to be renamed:
	   (let-values (((t pred pure) (validate-type new name)))
	     (unless t
	       (warning
		(sprintf "invalid type specification for `~a': ~a"
			 name
			 (type-name new))))
	     (when (and old (not (compatible-types? old t)))
	       (warning
		(sprintf
		 "type definition for toplevel binding `~a' \
		  conflicts with previously loaded type:\
		  ~n  New type:      ~a\
		  ~n  Original type: ~a"
		 name (type-name old) (type-name new))))
	     (mark-variable name '##compiler#type t)
	     (mark-variable name '##compiler#type-source 'db)
	     (when specs
	       (install-specializations name specs)))))
       (call-with-input-file dbfile read-expressions))
      #t)))

(define (emit-types-file source-file types-file db block-compilation)
  (with-output-to-file types-file
    (lambda ()
      (print "; GENERATED BY CHICKEN " (chicken-version) " FROM "
	     source-file "\n")
      (hash-table-for-each
       (lambda (sym plist)
	 (when (and (variable-visible? sym block-compilation)
		    (memq (variable-mark sym '##compiler#type-source) '(local inference)))
	   (let ((specs (or (variable-mark sym '##compiler#specializations) '()))
		 (type (variable-mark sym '##compiler#type))
		 (pred (variable-mark sym '##compiler#predicate))
		 (pure (variable-mark sym '##compiler#pure))
		 (clean (variable-mark sym '##compiler#clean))
		 (enforce (variable-mark sym '##compiler#enforce))
		 (foldable (variable-mark sym '##compiler#foldable)))
	     (pp (cons*
		  sym
		  (let wrap ((type type))
		    (if (pair? type)
			(case (car type)
			  ((procedure)
			   `(#(procedure
			       ,@(if enforce '(#:enforce) '())
			       ,@(if pred `(#:predicate ,pred) '())
			       ,@(if pure '(#:pure) '())
			       ,@(if clean '(#:clean) '())
			       ,@(if foldable '(#:foldable) '()))
			     ,@(cdr type)))
			  ((forall)
			   `(forall ,(second type) ,(wrap (third type))))
			  (else type))
			type))
		  specs))
	     (newline))))
       db)
      (print "; END OF FILE"))))

;;
;; Source node tracking
;;
;; Nodes are mutated in place during specialization, which may lose line
;; number information if, for example, a node is changed from a
;; ##core#call to a class without debug info. To preserve line numbers
;; and allow us to print fragments of the original source, we maintain a
;; side table of mappings from mutated nodes to copies of the originals.
;;

(define node-mutations '())

(define (mutate-node! node expr)
  (set! node-mutations (alist-update! node (copy-node node) node-mutations))
  (copy-node! (build-node-graph expr) node))

(define (source-node n #!optional (k values))
  (let ((orig (alist-ref n node-mutations eq?)))
    (if (not orig) (k n) (source-node orig k))))

(define (source-node-tree n)
  (source-node
   n
   (lambda (n*)
     (make-node (node-class n*)
		(node-parameters n*)
		(map source-node (node-subexpressions n*))))))

(define (node-line-number n)
  (node-debug-info (source-node n)))

(define (node-debug-info n)
  (case (node-class n)
    ((##core#call)
     (let ((params (node-parameters n)))
       (and (pair? (cdr params))
	    (pair? (cadr params)) ; debug-info has line-number information?
	    (source-info->line (cadr params)))))
    ((##core#typecase)
     (car (node-parameters n)))
    (else #f)))

;; Mutate node for specialization

(define (specialize-node! node args template)
  (let ((env '()))
    (define (subst x)
      (cond ((and (vector? x)
		  (= 1 (vector-length x)) )
	     (let ((y (vector-ref x 0)))
	       (cond ((integer? y)
		      (if (negative? y)
			  (list-tail args (sub1 (- y)))
			  (list-ref args (sub1 y))))
		     ((symbol? y)
		      (cond ((assq y env) => cdr)
			    (else
			     (let ((var (gensym y)))
			       (set! env (alist-cons y var env))
			       var)))))))
	    ((and (vector? x)
		  (= 2 (vector-length x))
		  (integer? (vector-ref x 0))
		  (eq? '... (vector-ref x 1)))
	     (list-tail args (sub1 (vector-ref x 0))))
	    ((not (pair? x)) x)
	    ((eq? 'quote (car x)) x)	; to handle numeric constants
	    (else (cons (subst (car x)) (subst (cdr x))))))
    (mutate-node! node (subst template))))


;;; Type-validation and -normalization

(define (validate-type type name)
  ;; - returns converted type or #f
  ;; - also converts "(... -> ...)" types
  ;; - converts some typenames to struct types (u32vector, etc.)
  ;; - handles some type aliases
  ;; - drops "#!key ..." args by converting to #!rest
  ;; - replaces uses of "&rest"/"&optional" with "#!rest"/"#!optional"
  ;; - handles "(T1 -> T2 : T3)" (predicate) 
  ;; - handles "(T1 --> T2 [: T3])" (clean)
  ;; - simplifies result
  ;; - coalesces all "forall" forms into one (remove "forall" if typevar-set is empty)
  ;; - renames type-variables
  ;; - replaces type-abbreviations
  (let ((ptype #f)			; (T . PT) | #f
	(clean #f)
	(typevars '())
	(constraints '()))
    (define (upto lst p)
      (let loop ((lst lst))
	(cond ((eq? lst p) '())
	      (else (cons (car lst) (loop (cdr lst)))))))
    (define (memq* x lst) ; memq, but allow improper list
      (let loop ((lst lst))
	(cond ((not (pair? lst)) #f)
	      ((eq? (car lst) x) lst)
	      (else (loop (cdr lst))))))
    (define (validate-llist llist)
      (cond ((null? llist) '())
	    ((symbol? llist) '(#!rest *))
	    ((not (pair? llist)) #f)
	    ((or (eq? '#!optional (car llist))
		 (eq? '&optional (car llist)))
	     (let ((l1 (validate-llist (cdr llist))))
	       (and l1 (cons '#!optional l1))))
	    ((or (eq? '#!rest (car llist))
		 (eq? '&rest (car llist)))
	     (cond ((null? (cdr llist)) '(#!rest *))
		   ((not (pair? (cdr llist))) #f)
		   (else
		    (let ((l1 (validate (cadr llist))))
		      (and l1 `(#!rest ,l1))))))
	    ((eq? '#!key (car llist)) '(#!rest *))
	    (else
	     (let* ((l1 (validate (car llist)))
		    (l2 (validate-llist (cdr llist))))
	       (and l1 l2 (cons l1 l2))))))
    (define (validate t #!optional (rec #t))
      (cond ((memq t value-types) t)
	    ((memq t basic-types) t)
	    ((memq t struct-types) `(struct ,t))
	    ((eq? t 'immediate) '(or eof null fixnum char boolean))
	    ((eq? t 'any) '*)
	    ((eq? t 'void) 'undefined)
	    ((eq? t 'input-port) '(refine (input) port))
	    ((eq? t 'output-port) '(refine (output) port))
	    ((and (symbol? t) (##sys#get t '##compiler#type-abbreviation)))
	    ((not (pair? t)) 
	     (cond ((memq t typevars) t)
		   (else #f)))
	    ((eq? 'not (car t))
	     (and (= 2 (length t))
		  `(not ,(validate (second t)))))
	    ((eq? 'forall (car t))
	     (and (= 3 (length t))
		  (list? (second t))
		  (call/cc
		   (lambda (return)
		     (set! typevars
		       (append (map (lambda (tv)
				      (cond ((symbol? tv) tv)
					    ((and (list? tv)
						  (= 2 (length tv))
						  (symbol? (car tv)))
					     (car tv))
					    (else (return #f))))
				    (second t))
			       typevars))
		     (set! constraints
		       (append (filter-map
				(lambda (tv)
				  (and (pair? tv)
				       (list (car tv)
					     (let ((t (validate (cadr tv))))
					       (unless t (return #f))
					       t))))
				(second t))
			       constraints))
		     (validate (third t) rec)))))
	    ((and (eq? 'quote (car t))
		  (pair? (cdr t))
		  (symbol? (second t))
		  (null? (cddr t))
		  (second t))
	     => (lambda (v)
		  (unless (memq v typevars)
		    (set! typevars (cons v typevars)))
		  v))
	    ((eq? 'or (car t)) 
	     (and (list? t)
		  (let ((ts (map validate (cdr t))))
		    (and (every identity ts)
			 `(or ,@ts)))))
	    ((eq? 'struct (car t))
	     (and (= 2 (length t)) (symbol? (second t)) t))
	    ((eq? 'deprecated (car t))
	     (and (= 2 (length t)) (symbol? (second t)) t))
	    ((eq? 'refine (car t))
	     (and (= 3 (length t))
		  (let ((t2 (validate (third t))))
		    (and (value-type? t2)
			 (list? (second t))
			 (every symbol? (second t))
			 (list 'refine (second t) t2)))))
	    ((or (memq* '--> t) (memq* '-> t)) =>
	     (lambda (p)
	       (let* ((cleanf (eq? '--> (car p)))
		      (ok (or (not rec) (not cleanf))))
		 (unless rec (set! clean cleanf))
		 (let ((cp (memq* ': p)))
		   (cond ((not cp)
			  (and ok
			       (validate
				`(procedure ,(upto t p) ,@(cdr p))
				rec)))
			 ((and (= 5 (length t))
			       (eq? p (cdr t)) ; one argument?
			       (eq? cp (cdddr t))) ; 4th item is ":"?
			  (set! t (validate `(procedure (,(first t)) ,(third t)) rec))
			  ;; we do it this way to distinguish the "outermost" predicate
			  ;; procedure type
			  (set! ptype (cons t (validate (cadr cp))))
			  (and ok t))
			 (else #f))))))
	    ((memq (car t) '(vector-of list-of))
	     (and (list? t)
		  (= 2 (length t))
		  (let ((t2 (validate (second t))))
		    (and t2 `(,(car t) ,t2)))))
	    ((memq (car t) '(vector list))
	     (and (list? t)
		  (let loop ((ts (cdr t)) (ts2 '()))
		    (cond ((null? ts) `(,(car t) ,@(reverse ts2)))
			  ((validate (car ts)) => 
			   (lambda (t2) (loop (cdr ts) (cons t2 ts2))))
			  (else #f)))))
	    ((eq? 'pair (car t))
	     (and (= 3 (length t))
		  (let ((ts (map validate (cdr t))))
		    (and (every identity ts) `(pair ,@ts)))))
	    ((eq? 'procedure (car t))
	     (and (pair? (cdr t))
		  (let* ((name (if (symbol? (cadr t))
				   (cadr t)
				   name))
			 (t2 (if (symbol? (cadr t)) (cddr t) (cdr t))))
		    (and (pair? t2)
			 (list? (car t2))
			 (let ((ts (validate-llist (car t2))))
			   (and ts
				(every identity ts)
				(let* ((rt2 (cdr t2))
				       (rt (if (eq? '* rt2) 
					       rt2
					       (and (list? rt2)
						    (let ((rts (map validate rt2)))
						      (and (every identity rts)
							   rts))))))
				  (and rt
				       `(procedure 
					 ,@(if (and name (not rec)) (list name) '())
					 ,ts
					 ,@rt)))))))))
	    (else #f)))
    (cond ((validate type #f) =>
	   (lambda (type)
	     (when (pair? typevars)
	       (set! type
		 `(forall
		   ,(map (lambda (tv)
			   (cond ((assq tv constraints) => identity)
				 (else tv)))
			 (delete-duplicates typevars eq?))
		   ,type)))
	     (let ((type2 (simplify-type type)))
	       (values 
		type2
		(and ptype (eq? (car ptype) type) (cdr ptype))
		clean))))
	  (else (values #f #f #f)))))

(define (check-and-validate-type type loc #!optional name)
  (let-values (((t pred pure) (validate-type (strip-syntax type) name)))
    (or t 
	(error loc "invalid type specifier" type))))

(define (install-specializations name specs)
  (define (fail spec)
    (error "invalid specialization format" spec name))
  (mark-variable 
   name '##compiler#specializations
   ;;XXX it would be great if result types could refer to typevars
   ;;    bound in the argument types, like this:
   ;;
   ;; (: with-input-from-file ((-> . *) -> . *)
   ;;    (((forall (a) (-> a))) (a) ...code that does it single-valued-ly...))
   ;;
   ;; This would make it possible to propagate the (single) result type from
   ;; the thunk to the enclosing expression. Unfortunately the simplification in
   ;; the first validation renames typevars, so the second validation will have
   ;; non-matching names.
   (map (lambda (spec)
	  (if (and (list? spec) (list? (first spec)))
	      (let* ((args
		      (map (lambda (t) 
			     (let-values (((t2 pred pure) (validate-type t #f)))
			       (or t2
				   (error "invalid argument type in specialization" 
					  t spec name))))
			   (first spec)))
		     (typevars (unzip1 (append-map type-typeenv args))))
		(cons
		 args
		 (case (length spec)
		   ((2) (cdr spec))
		   ((3) 
		    (cond ((list? (second spec))
			   (cons
			    (map (lambda (t)
				   (let-values (((t2 pred pure) (validate-type t #f)))
				     (or t2
					 (error "invalid result type in specialization" 
						t spec name))))
				 (second spec))
			    (cddr spec)))
			  ((eq? '* (second spec)) (cdr spec))
			  (else (fail spec))))
		   (else (fail spec)))))
	      (fail spec)))
	specs)))


;;; Canonicalize complex pair/list type for matching with "list-of"
;
; Returns an equivalent (list ...) form, or the original argument if no
; canonicalization could be done.

(define (canonicalize-list-type t)
  (cond ((not (pair? t)) t)
	((eq? 'pair (car t))
	 (let ((tcar (second t))
	       (tcdr (third t)))
	   (let rec ((tr tcdr) (ts (list tcar)))
	     (cond ((eq? 'null tr)
		    `(list ,@(reverse ts)))
		   ((and (pair? tr) (eq? 'pair (first tr)))
		    (rec (third tr) (cons (second tr) ts)))
		   ((and (pair? tr) (eq? 'list (first tr)))
		    `(list ,@(reverse ts) ,@(cdr tr)))
		   (else t)))))
	(else t)))


;;; Drop namespace from module-prefixed symbol:

(define (strip-namespace sym)
  (let* ((s (symbol->string sym))
	 (n (string-length s)))
    (let loop ((i 0))
      (cond ((eq? i n) sym)
	    ((eq? (##core#inline "C_subchar" s i) #\#)
	     (##sys#intern-symbol (##sys#substring s (fx+ i 1) n)))
	    (else (loop (fx+ i 1)))))))


;;; hardcoded result types for certain primitives

(define-syntax define-special-case
  (syntax-rules ()
    ((_ name handler)
     (##sys#put! 'name '##compiler#special-result-type handler))))

(define-special-case ##sys#make-structure
  (lambda (node args loc rtypes)
    (or (and-let* ((subs (node-subexpressions node))
                   ((>= (length subs) 2))
                   (arg1 (second subs))
                   ((eq? 'quote (node-class arg1)))
                   (val (first (node-parameters arg1)))
                   ((symbol? val)))
          ;;XXX a dirty hack - we should remove the distinct
          ;;    "pointer-vector" type.
          (if (eq? 'pointer-vector val)
              '(pointer-vector)
              `((struct ,(strip-namespace val)))))
	rtypes)))

(let ()
  ;; TODO: Complain argument not available here, so we can't use the
  ;; standard "report" defined above.  However, ##sys#enable-warnings
  ;; and "complain" (do-scrutinize) are always true together, except
  ;; that "complain" will be false while ##sys#enable-warnings is true
  ;; on "no-usual-integrations", so perhaps get rid of "complain"?
  (define (report loc msg . args)
    (warning
     (conc (location-name loc)
	   (sprintf "~?" msg (map type-name args)))))

  (define (known-length-vector-index node args loc expected-argcount)
    (and-let* ((subs (node-subexpressions node))
	       ((= (length subs) (add1 expected-argcount)))
	       (arg1 (walked-result (second args)))
	       ((pair? arg1))
	       ((eq? 'vector (car arg1)))
	       (index (third subs))
	       ((eq? 'quote (node-class index)))
	       (val (first (node-parameters index)))
	       ((fixnum? val)) ; Standard type warning otherwise
	       (vector-length (length (cdr arg1))))
      (if (and (>= val 0) (< val vector-length))
	  val
	  (begin
	    (report
	     loc "~ain procedure call to `~a', index ~a out of range \
                   for vector of length ~a"
	     (node-source-prefix node)
	     ;; TODO: It might make more sense to use "pname" here
	     (first (node-parameters (first subs))) val vector-length)
	    #f))))

  ;; These are a bit hacky, since they mutate the node.  These special
  ;; cases are really only intended for determining result types...
  (define (vector-ref-result-type node args loc rtypes)
    (or (and-let* ((index (known-length-vector-index node args loc 2))
		   (arg1 (walked-result (second args)))
		   (vector (second (node-subexpressions node))))
	  (mutate-node! node `(##sys#slot ,vector ',index))
	  (list (list-ref (cdr arg1) index)))
	rtypes))

  (define-special-case scheme#vector-ref vector-ref-result-type)
  (define-special-case ##sys#vector-ref vector-ref-result-type)

  (define-special-case scheme#vector-set!
    (lambda (node args loc rtypes)
      (or (and-let* ((index (known-length-vector-index node args loc 3))
		     (subs (node-subexpressions node))
		     (vector (second subs))
		     (new-value (fourth subs))
		     (new-value-type (walked-result (fourth args)))
		     (setter (if (type-always-immediate? new-value-type)
				 '##sys#setislot
				 '##sys#setslot)))
	    (mutate-node! node `(,setter ,vector ',index ,new-value))
	    '(undefined))
	  rtypes))))

;; TODO: Also special-case vector-length?  Makes little sense though.


;;; List-related special cases
;
; Preserve known element types for:
;
;   list-ref, list-tail

(let ()
  ;; See comment in vector (let) just above this
  (define (report loc msg . args)
    (warning
     (conc (location-name loc)
	   (sprintf "~?" msg (map type-name args)))))

  (define (list-or-null a)
    (if (null? a) 'null `(list ,@a)))

  ;; Split a list or pair type form at index i, calling k with the two
  ;; sections of the type or returning #f if it doesn't match that far.
  ;; Note that "list-of" is handled by "forall" entries in types.db
  (define (split-list-type l i k)
    (cond ((not (pair? l))
	   (and (fx= i 0) (eq? l 'null) (k l l)))
	  ((eq? (first l) 'list)
	   (and (fx< i (length l))
		(receive (left right) (split-at (cdr l) i)
		  (k (list-or-null left)
		     (list-or-null right)))))
	  ((eq? (first l) 'pair)
	   (let lp ((a '()) (l l) (i i))
	     (cond ((fx= i 0)
		    (k (list-or-null (reverse a)) l))
		   ((and (pair? l)
			 (eq? (first l) 'pair))
		    (lp (cons (second l) a)
                        (third l)
                        (sub1 i)))
		   (else #f))))
	  (else #f)))

  ;; canonicalize-list-type will have taken care of converting (pair
  ;; (pair ...)) to (list ...) or (list-of ...) for proper lists.
  (define (proper-list-type-length t)
    (cond ((eq? t 'null) 0)
	  ((and (pair? t) (eq? (car t) 'list)) (length (cdr t)))
	  (else #f)))

  (define (list+index-call-result-type-special-case k)
    (lambda (node args loc rtypes)
      (or (and-let* ((subs (node-subexpressions node))
		     ((= (length subs) 3))
		     (arg1 (walked-result (second args)))
		     (index (third subs))
		     ((eq? 'quote (node-class index)))
		     (val (first (node-parameters index)))
		     ((fixnum? val))) ; Standard type warning otherwise
	    ;; TODO: It might make sense to use "pname" when reporting
	    (cond ((negative? val)
		   ;; Negative indices should always generate a warning
		   (report
		    loc "~ain procedure call to `~a', index ~a is \
                        negative, which is never valid"
		    (node-source-prefix node)
		    (first (node-parameters (first subs))) val)
		   #f)
		  ((split-list-type arg1 val k))
		  ;; Warn only if it's a known proper list.  This avoids
		  ;; false warnings due to component smashing.
		  ((proper-list-type-length arg1) =>
		   (lambda (length)
		     (report
		      loc "~ain procedure call to `~a', index ~a out of \
                        range for proper list of length ~a"
		      (node-source-prefix node)
		      (first (node-parameters (first subs))) val length)
		     #f))
		  (else #f)))
	  rtypes)))

  (define-special-case scheme#list-ref
    (list+index-call-result-type-special-case
     (lambda (_ result-type)
       (and (pair? result-type)
	    (list (cadr result-type))))))

  (define-special-case scheme#list-tail
    (list+index-call-result-type-special-case
     (lambda (_ result-type) (list result-type)))))

(define-special-case scheme#list
  (lambda (node args loc rtypes)
    (if (null? (cdr args))
	'(null)
	`((list ,@(map walked-result (cdr args)))))))

(define-special-case ##sys#list
  (lambda (node args loc rtypes)
    (if (null? (cdr args))
	'(null)
	`((list ,@(map walked-result (cdr args)))))))

(define-special-case scheme#vector
  (lambda (node args loc rtypes)
    `((vector ,@(map walked-result (cdr args))))))

(define-special-case ##sys#vector
  (lambda (node args loc rtypes)
    `((vector ,@(map walked-result (cdr args))))))

(define-special-case scheme#reverse
  (lambda (node args loc rtypes)
    (or (and-let* ((subs (node-subexpressions node))
		   ((= (length subs) 2))
		   (arg1 (walked-result (second args)))
		   ((pair? arg1))
		   ((eq? (car arg1) 'list)))
	  `((list ,@(reverse (cdr arg1)))))
	rtypes)))

(let ()
  ;; See comment in vector (let)
  (define (report loc msg . args)
    (warning
     (conc (location-name loc)
	   (sprintf "~?" msg (map type-name args)))))

  (define (append-special-case node args loc rtypes)
    (define (potentially-proper-list? l) (match-types l 'list '()))

    (define (derive-result-type)
      (let lp ((arg-types (cdr args))
	       (index 1))
	(if (null? arg-types)
	    'null
	    (let ((arg1 (walked-result (car arg-types))))
	      (cond
	       ((and (pair? arg1) (eq? (car arg1) 'list))
		(and-let* ((rest-t (lp (cdr arg-types) (add1 index))))
		  ;; decanonicalize, then recanonicalize to make it
		  ;; easy to append a variety of types.
		  (canonicalize-list-type
		   (foldl (lambda (rest t) `(pair ,t ,rest))
			  rest-t (reverse (cdr arg1))))))

	       ((and (pair? arg1) (eq? (car arg1) 'list-of))
		(and-let* ((rest-t (lp (cdr arg-types) (add1 index))))
		  ;; list-of's length unsurety is "contagious"
		  (simplify-type `(or ,arg1 ,rest-t))))

	       ;; TODO: (append (pair x (pair y z)) lst) =>
	       ;; (pair x (pair y (or z lst)))
	       ;; This is trickier than it sounds!

	       (else
		;; The final argument may be an atom or improper list
		(unless (or (null? (cdr arg-types))
			    (potentially-proper-list? arg1))
		  (report
		   loc "~ain procedure call to `~a', argument #~a is \
			of type ~a but expected a proper list"
		   (node-source-prefix node)
		   (first (node-parameters
			   (first (node-subexpressions node))))
		   index arg1))
		#f))))))
    (cond ((derive-result-type) => list)
	  (else rtypes)))

  (define-special-case scheme#append append-special-case)
  (define-special-case ##sys#append append-special-case))

;;; Special cases for make-list/make-vector with a known size
;
; e.g. (make-list 3 #\a) => (list char char char)

(let ()

  (define (complex-object-constructor-result-type-special-case type)
    (lambda (node args loc rtypes)
      (or (and-let* ((subs (node-subexpressions node))
		     (fill (case (length subs)
			     ((2) '*)
			     ((3) (walked-result (third args)))
			     (else #f)))
		     (sub2 (second subs))
		     ((eq? 'quote (node-class sub2)))
		     (size (first (node-parameters sub2)))
		     ((fixnum? size))
		     ((<= 0 size +maximal-complex-object-constructor-result-type-length+)))
	    `((,type ,@(make-list size fill))))
	  rtypes)))

  (define-special-case scheme#make-vector
    (complex-object-constructor-result-type-special-case 'vector)))


;;; perform check over all typevar instantiations
;
; If "all" is #t all types in tlist must match, if #f then one or more.

(define (over-all-instantiations tlist typeenv all process)
  (let ((insts '())
	(anyinst #f)
	(trail0 trail))

    ;; restore trail and collect instantiations
    (define (restore)
      (ddd "restoring, trail: ~s, te: ~s" trail typeenv)
      (let ((is '()))
	(do ((tr trail (cdr tr)))
	    ((eq? tr trail0)
	     (set! trail tr)
	     (when (pair? is) (set! anyinst #t))
	     (set! insts (cons is insts)))
	  (set! is (alist-cons 
		    (car tr)
		    (resolve (car tr) typeenv)
		    is))
	  (ddd "  restoring ~a, insts: ~s" (car tr) insts)
	  (let ((a (assq (car tr) typeenv)))
	    (set-car! (cdr a) #f)))))

    ;; collect candidates for each typevar
    (define (collect)
      (let* ((vars (delete-duplicates (concatenate (map unzip1 insts)) eq?))
	     (all (map (lambda (var)
			 (cons
			  var
			  (filter-map
			   (lambda (inst)
			     (cond ((assq var inst) => cdr)
				   ;;XXX is the following correct in all cases?
				   (all '*)
				   (else #f)))
			   insts)))
		       vars)))
	(ddd "  collected: ~s" all)
	all))

    (ddd " over-all-instantiations: ~s all: ~a" tlist all)
    ;; process all tlist elements
    (let loop ((ts (delete-duplicates tlist eq?))
	       (ok #f))
      (cond ((null? ts)
	     (cond ((or ok (null? tlist))
		    (for-each 
		     (lambda (i)
		       (set! trail (cons (car i) trail))
		       (set-car! (cdr (assq (car i) typeenv))
				 (simplify-type `(or ,@(cdr i)))))
		     (collect))
		    #t)
		   (else #f)))
	    ((process (car ts))
	     (restore)
	     (loop (cdr ts) #t))
	    (all
	     (restore)
	     #f)
	    (else 
	     (restore)
	     (loop (cdr ts) ok))))))
)
