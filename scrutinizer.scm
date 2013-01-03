;;;; scrutinizer.scm - The CHICKEN Scheme compiler (local flow analysis)
;
; Copyright (c) 2009-2013, The Chicken Team
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
  (hide specialize-node! specialization-statistics
	procedure-type? named? procedure-result-types procedure-argument-types
	noreturn-type? rest-type procedure-name d-depth
	noreturn-procedure-type? trail trail-restore walked-result 
	typename multiples procedure-arguments procedure-results
	smash-component-types! generate-type-checks! over-all-instantiations
	compatible-types? type<=? match-types resolve match-argument-types))


(include "compiler-namespace")
(include "tweaks")


(define d-depth 0)
(define scrutiny-debug #t)

(define (d fstr . args)
  (when (and scrutiny-debug (##sys#fudge 13))
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
;   SPEC = * | (VAL1 ...)
;   VAL = (or VAL1 ...)
;       | (not VAL)       
;       | (struct NAME)
;       | (procedure [NAME] (VAL1 ... [#!optional VALOPT1 ...] [#!rest [VAL | values]]) . RESULTS)
;       | BASIC
;       | COMPLEX
;       | (forall (TVAR1 ...) VAL)
;       | deprecated
;       | (deprecated NAME)
;   BASIC = * | string | symbol | char | number | boolean | list | pair | 
;           procedure | vector | null | eof | undefined | input-port | output-port |
;           blob | noreturn | pointer | locative | fixnum | float |
;           pointer-vector
;   COMPLEX = (pair VAL VAL)
;           | (vector-of VAL) 
;           | (list-of VAL)
;           | (vector VAL1 ...)
;           | (list VAL1 ...)
;   RESULTS = * 
;           | (VAL1 ...)
;   TVAR = (VAR TYPE) | VAR
;
; global symbol properties:
;
;   ##compiler#type            ->  TYPESPEC
;   ##compiler#declared-type   ->  BOOL
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
;   SPECIALIZATION = ((VAL ... [#!rest VAL]) [RESULTS] TEMPLATE)
;   TEMPLATE = #(INDEX)
;            | #(INDEX ...)
;            | #(SYMBOL)
;            | INTEGER | SYMBOL | STRING
;            | (quote CONSTANT)
;            | (TEMPLATE . TEMPLATE)


(define-constant +fragment-max-length+ 6)
(define-constant +fragment-max-depth+ 4)
(define-constant +maximal-union-type-length+ 20)


(define specialization-statistics '())
(define trail '())


(define (multiples n)
  (if (= n 1) "" "s"))

(define (walked-result n)
  (first (node-parameters n)))		; assumes ##core#the/result node


(define (scrutinize node db complain specialize)
  (let ((blist '())			; (((VAR . FLOW) TYPE) ...)
	(aliased '())
	(noreturn #f)
	(dropped-branches 0)
	(errors #f)
	(safe-calls 0))

    (define (constant-result lit)
      (cond ((string? lit) 'string)
	    ((symbol? lit) 'symbol)
	    ((fixnum? lit) 'fixnum)
	    ((flonum? lit) 'float)
	    ((number? lit) 
	     (case number-type 
	       ((fixnum) 'fixnum)
	       ((flonum) 'flonum)
	       (else 'number)))		; in case...
	    ((boolean? lit) 'boolean)
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
		 (report
		  loc
		  (sprintf "use of deprecated library procedure `~a'" id) )
		 '(*))
		((and (pair? a) (eq? (car a) 'deprecated))
		 (report
		  loc
		  (sprintf 
		      "use of deprecated library procedure `~a' - consider using `~a' instead"
		    id (cadr a)))
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
	    ((and (not strict-variable-types)
		  (get db id 'assigned) 
		  (not (variable-mark id '##compiler#declared-type)))
	     '(*))
	    ((assq id e) =>
	     (lambda (a)
	       (cond ((eq? 'undefined (cdr a))
		      #;(report 
		       loc
		       (sprintf "access to variable `~a' which has an undefined value"
			 (real-name id db)))
		      '(*))
		     (else (list (cdr a))))))
	    (else (global-result id loc))))

    (define (always-true1 t)
      (cond ((pair? t)
	     (case (car t)
	       ((or) (every always-true1 (cdr t)))
	       ((forall) (always-true1 (third t)))
	       (else #t)))
	    ((memq t '(* boolean undefined noreturn)) #f)
	    (else #t)))

    (define (always-true t loc x)
      (let ((f (always-true1 t)))
	(when f
	  (report-notice 
	   loc
	   (sprintf
	       "expected value of type boolean in conditional but were given a value of type\n  `~a' which is always true:~%~%~a"
	     t
	     (pp-fragment x))))
	f))

    (define (single what tv loc)
      (if (eq? '* tv)
	  '*
	  (let ((n (length tv)))
	    (cond ((= 1 n) (car tv))
		  ((zero? n)
		   (report
		    loc
		    (sprintf "expected ~a a single result, but were given zero results" what))
		   'undefined)
		  (else
		   (report
		    loc
		    (sprintf "expected ~a a single result, but were given ~a result~a"
		      what n (multiples n)))
		   (first tv))))))

    (define (report-notice loc desc #!optional (show complain))
      (when show
	(##sys#notice
	 (conc (location-name loc) desc))))

    (define (report loc desc #!optional (show complain))
      (when show
	(warning
	 (conc (location-name loc) desc))))

    (define (report-error loc desc #!optional (show complain))
      (when show
	(warning 
	 (conc (location-name loc) desc)))
      (set! errors #t))

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

    (define add-loc cons)

    (define (fragment x)
      (let ((x (build-expression-tree x)))
	(let walk ((x x) (d 0))
	  (cond ((atom? x) x)
		((>= d +fragment-max-depth+) '...)
		((list? x)
		 (let* ((len (length x))
			(xs (if (< +fragment-max-length+ len)
				(append (take x +fragment-max-length+) '(...))
				x)))
		   (map (cute walk <> (add1 d)) xs)))
		(else x)))))

    (define (pp-fragment x)
      (string-chomp
       (with-output-to-string
	 (lambda ()
	   (pp (fragment x))))))

    (define (get-specializations name)
      (let* ((a (variable-mark name '##compiler#specializations))
	     (b (variable-mark name '##compiler#local-specializations))
	     (c (append (or a '()) (or b '()))))
	(and (pair? c) c)))

    (define (call-result node args e loc params typeenv)
      (define (pname)
	(sprintf "~ain procedure call to `~s', " 
	  (if (and (pair? params)
		   (pair? (cdr params))
		   (pair? (cadr params))) ; sourceinfo has line-number information?
	      (let ((n (source-info->line (cadr params))))
		(if n
		    (sprintf "(~a) " n)
		    ""))
	      "")
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
		(sprintf
		    "~aexpected a value of type `~a', but was given a value of type `~a'"
		  (pname) 
		  (resolve xptype typeenv)
		  (resolve ptype typeenv)))
	       (values '* #f))
	      (else
	       (let-values (((atypes values-rest ok alen)
			     (procedure-argument-types ptype nargs typeenv)))
		 (unless ok
		   (report 
		    loc
		    (sprintf
			"~aexpected ~a argument~a, but was given ~a argument~a"
		      (pname)
		      alen (multiples alen)
		      nargs (multiples nargs))))
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
		      (sprintf
			  "~aexpected argument #~a of type `~a', but was given an argument of type `~a'"
			(pname) 
			i
			(resolve (car atypes) typeenv)
			(resolve (car actualtypes) typeenv)))))
		 (when (noreturn-procedure-type? ptype)
		   (set! noreturn #t))
		 (let ((r (procedure-result-types ptype values-rest (cdr actualtypes) typeenv)))
		   (let* ((pn (procedure-name ptype))
			  (trail0 trail))
		     (when pn
		       (cond ((and (fx= 1 nargs) 
				   (variable-mark pn '##compiler#predicate)) =>
				   (lambda (pt)
				     (cond ((match-argument-types
					     (list pt) (cdr actualtypes) typeenv #f #t)
					    (report-notice
					     loc
					     (sprintf 
						 "~athe predicate is called with an argument of type\n  `~a' and will always return true"
					       (pname) (cadr actualtypes)))
					    (when specialize
					      (specialize-node!
					       node (cdr args)
					       `(let ((#(tmp) #(1))) '#t))
					      (set! op (list pn pt))))
					   ((begin
					      (trail-restore trail0 typeenv)
					      (match-argument-types
					       (list `(not ,pt)) (cdr actualtypes) typeenv #f #t))
					    (report-notice
					     loc
					     (sprintf 
						 "~athe predicate is called with an argument of type\n  `~a' and will always return false"
					       (pname) (cadr actualtypes)))
					    (when specialize
					      (specialize-node!
					       node (cdr args)
					       `(let ((#(tmp) #(1))) '#f))
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
					 (cond ((match-argument-types
						 stype (cdr actualtypes) tenv2
						 #t)
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
	(set! blist (alist-cons (cons var flow) type blist))
	(let ((a (assq var aliased)))
	  (when a
	    (d "  applying to alias: ~a -> ~a" (cdr a) type)
	    (loop (cdr a))))))

    (define (initial-argument-types dest vars argc)
      (if (and dest 
	       strict-variable-types
	       (variable-mark dest '##compiler#declared-type))
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
		 ((##core#global-ref) (global-result (first params) loc))
		 ((##core#variable) (variable-result (first params) e loc flow))
		 ((if)
		  (let ((tags (cons (tag) (tag)))
			(tst (first subs))
			(nor-1 noreturn))
		    (set! noreturn #f)
		    (let* ((rt (single "in conditional" (walk tst e loc #f #f flow tags) loc))
			   (c (second subs))
			   (a (third subs))
			   (nor0 noreturn))
		      (when (and (always-true rt loc n) specialize)
			(set! dropped-branches (add1 dropped-branches))
			(copy-node!
			 (build-node-graph
			  `(let ((,(gensym) ,tst)) ,c))
			 n))
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
					 (sprintf
					     "branches in conditional expression differ in the number of results:~%~%~a"
					   (pp-fragment n)))
					'*)
				       (nor1 r2)
				       (nor2 r1)
				       (else
					(dd "merge branch results: ~s + ~s" r1 r2)
					(map (lambda (t1 t2)
					       (simplify-type `(or ,t1 ,t2)))
					     r1 r2))))
				(else '*)))))))
		 ((let)
		  ;; before CPS-conversion, `let'-nodes may have multiple bindings
		  (let loop ((vars params) (body subs) (e2 '()))
		    (if (null? vars)
			(walk (car body) (append e2 e) loc dest tail flow ctags)
			(let* ((var (car vars))
			       (val (car body))
			       (t (single 
				   (sprintf "in `let' binding of `~a'" (real-name var))
				   (walk val e loc var #f flow #f) 
				   loc)))
			  (when (and (eq? (node-class val) '##core#variable)
				     (not (get db var 'assigned)))
			    (let ((var2 (first (node-parameters val))))
			      (unless (get db var2 'assigned) ;XXX too conservative?
				(set! aliased (alist-cons var var2 aliased)))))
			  (loop (cdr vars) (cdr body) (alist-cons (car vars) t e2))))))
		 ((##core#lambda lambda)
		  (decompose-lambda-list
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
				      (variable-mark dest '##compiler#declared-type)
				      (not unsafe))
			     (debugging 'x "checks argument-types" dest) ;XXX
			     ;; [1] this is subtle: we don't want argtype-checks to be 
			     ;; generated for toplevel defs other than user-declared ones. 
			     ;; But since the ##compiler#declared-type mark is set AFTER 
			     ;; the lambda has been walked (see below, [2]), nothing is added.
			     (generate-type-checks! n dest vars inits))
			   (list
			    (append
			     '(procedure) 
			     namelst
			     (list
			      (let loop ((argc argc) (vars vars) (args args))
				(cond ((zero? argc) args)
				      ((and (not (get db (car vars) 'assigned))
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
		      ((if strict-variable-types report-error report)
		       loc
		       (sprintf 
			   "assignment of value of type `~a' to toplevel variable `~a' does not match declared type `~a'"
			 rt var type)
		       #t))
		    (when (and (not type) ;XXX global declaration could allow this
			       (not b)
			       (not (eq? '* rt))
			       (not (get db var 'unknown)))
		      (and-let* ((val (or (get db var 'value)
					  (get db var 'local-value))))
			(when (and (eq? val (first subs))
				   (or (not (variable-visible? var))
				       (not (eq? (variable-mark var '##compiler#inline) 
						 'no))))
			  (let ((rtlst (list (cons #f (tree-copy rt)))))
			    (smash-component-types! rtlst "global")
			    (let ((rt (cdar rtlst)))
			      (debugging '|I| (sprintf "(: ~s ~s)" var rt))
			      ;; [2] sets property, but lambda has already been walked,
			      ;; so no type-checks are generated (see also [1], above)
			      ;; note that implicit declarations are not enforcing
			      (mark-variable var '##compiler#declared-type)
			      (mark-variable var '##compiler#type rt))))))
		    (when b
		      (cond ((eq? 'undefined (cdr b)) (set-cdr! b rt))
			    #;(strict-variable-types
			     (let ((ot (or (blist-type var flow) (cdr b))))
			       ;;XXX compiler-syntax for "map" will introduce
			       ;;    assignments that trigger this warning, so this
			       ;;    is currently disabled
			       (unless (compatible-types? ot rt)
				 (report
				  loc
				  (sprintf 
				      "variable `~a' of type `~a' was modified to a value of type `~a'"
				    var ot rt)
				  #t)))))
		      ;; don't use "add-to-blist" since the current operation does not affect aliases
		      (let ((t (if (or strict-variable-types
				       (not (get db var 'captured)))
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
		    '(undefined)))
		 ((##core#primitive ##core#inline_ref) '*)
		 ((##core#call)
		  (let* ((f (fragment n))
			 (len (length subs))
			 (args (map (lambda (n i)
				      (make-node
				       '##core#the/result
				       (list
					(single 
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
			(when (and (not strict-variable-types)
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
					 (a (assq var e))
					 (argr (resolve argr typeenv))
					 (oparg? (eq? arg (first subs)))
					 (pred (and pt
						    ctags
						    (not (get db var 'assigned)) 
						    (not oparg?))))
				    (cond (pred
					   (let ((pt (resolve pt typeenv)))
					     (d "  predicate `~a' indicates `~a' is ~a in flow ~a"
						pn var pt (car ctags))
					     (add-to-blist 
					      var (car ctags)
					      (if (and a (type<=? (cdr a) pt)) (cdr a) pt))))
					  (a
					   (when enforces
					     (let ((ar (cond ((blist-type var flow) =>
							      (lambda (t)
								(if (type<=? t argr)
								    t
								    argr)))
							     ((get db var 'assigned) '*)
							     ((type<=? (cdr a) argr) (cdr a))
							     (else argr))))
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
						(set! r (srt n args r))))))))
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
			    (sprintf
				"expression returns zero values but is declared to have a single result of type `~a'"
			      t)))
			  (else
			   (when (> (length rt) 1)
			     (report
			      loc
			      (sprintf 
				  "expression returns ~a values but is declared to have a single result"
				(length rt))))
			   (when (and (second params)
				      (not (type<=? t (first rt))))
			     ((if strict-variable-types report-error report-notice)
			      loc
			      (sprintf
				  "expression returns a result of type `~a', but is declared to return `~a', which is not a subtype"
				(first rt) t)))))
		    (list t)))
		 ((##core#typecase)
		  (let* ((ts (walk (first subs) e loc #f #f flow ctags))
			 (trail0 trail)
			 (typeenv (type-typeenv (car ts))))
		    ;; first exp is always a variable so ts must be of length 1
		    (let loop ((types (cdr params)) (subs (cdr subs)))
		      (cond ((null? types)
			     (quit "~a~ano clause applies in `compiler-typecase' for expression of type `~s':~a" 
				   (location-name loc)
				   (if (first params) 
				       (sprintf "(~a) " (first params))
				       "")
				   (car ts)
				   (string-concatenate
				    (map (lambda (t) (sprintf "\n    ~a" t))
					 (cdr params)))))
			    ((match-types (car types) (car ts) 
					  (append (type-typeenv (car types)) typeenv)
					  #t)
			     ;; drops exp
			     (copy-node! (car subs) n)
			     (walk n e loc dest tail flow ctags))
			    (else
			     (trail-restore trail0 typeenv)
			     (loop (cdr types) (cdr subs)))))))
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
      (when errors
	(quit "some variable types do not satisfy strictness"))
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
	  ((list-of list)
	   (dd "  smashing `~s' in ~a" (caar lst) where)
	   (change! '(or pair null))
	   (car t))
	  ((pair vector)
	   (dd "  smashing `~s' in ~a" (caar lst) where)
	   (change! (car t))
	   (car t))
	  ((forall)
	   (loop (third t)
		 (cute set-car! (cddr t) <>))))))))


;;; Converting type into string

(define (typename t)
  (define (argument-string args)
    (let* ((len (length (delete '#!optional args eq?)))
	   (m (multiples len)))
      ;;XXX not quite right for rest/optional arguments
      (cond ((memq '#!rest args)
	     (sprintf "~a or more arguments" len))
	    ((zero? len) "zero arguments")
	    (else
	     (sprintf 
		 "~a argument~a of type~a ~a"
	       len m m
	       (string-intersperse (map typename args) ", "))))))
  (define (result-string results)
    (if (eq? '* results) 
	"an unknown number of values"
	(let* ((len (length results))
	       (m (multiples len)))
	  (if (zero? len)
	      "zero values"
	      (sprintf 
		  "~a value~a of type~a ~a"
		len m m
		(string-intersperse (map typename results) ", "))))))
  (case t
    ((*) "anything")
    ((char) "character")
    (else
     (cond ((symbol? t) (symbol->string t))
	   ((pair? t)
	    (case (car t)
	      ((procedure) 
	       (if (or (string? (cadr t)) (symbol? (cadr t)))
		   (->string (cadr t))
		   (sprintf "a procedure with ~a returning ~a"
		     (argument-string (cadr t))
		     (result-string (cddr t)))))
	      ((or)
	       (string-intersperse
		(map typename (cdr t))
		" OR "))
	      ((struct)
	       (sprintf "a structure of type ~a" (cadr t)))
	      ((forall) 
	       (sprintf "~a (for all ~a)"
		 (typename (third t))
		 (string-intersperse
		  (map (lambda (tv)
			 (if (symbol? tv)
			     (symbol->string tv)
			     (sprintf "~a being ~a" (first tv) (typename (second tv)))))
		       (second t))
		  " ")))
	      ((not)
	       (sprintf "NOT ~a" (typename (second t))))
	      ((pair)
	       (sprintf "a pair wth car ~a and cdr ~a"
		 (typename (second t))
		 (typename (third t))))
	      ((vector-of)
	       (sprintf "a vector with element type ~a" (typename (second t))))
	      ((list-of)
	       (sprintf "a list with element type ~a" (typename (second t))))
	      ((vector list)
	       (sprintf "a ~a with the element types ~a"
		 (car t)
		 (map typename (cdr t))))
	      (else (bomb "typename: invalid type" t))))
	   (else (bomb "typename: invalid type" t))))))


;;; Type-matching
;
; - "exact" means: first argument must match second one exactly
; - "all" means: all elements in `or'-types in second argument must match

(define (match-types t1 t2 typeenv #!optional exact all)

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
    (let-values (((head tail) (break (cut eq? '#!rest <>) args)))
      (and (every			
	    (lambda (t)
	      (or (eq? '#!optional t)
		  (match1 rtype t)))
	    head)
	   (match1 rtype (if (pair? tail) (rest-type (cdr tail)) '*)))))

  (define (optargs? a)
    (memq a '(#!rest #!optional)))

  (define (match-results results1 results2)
    (cond ((null? results1) 
	   (or (null? results2)
	       (and (not exact) (eq? '* results2))))
	  ((eq? '* results1))
	  ((eq? '* results2) (not exact))
	  ((null? results2) #f)
	  ((and (memq (car results1) '(undefined noreturn))
		(memq (car results2) '(undefined noreturn))))
	  ((match1 (car results1) (car results2)) 
	   (match-results (cdr results1) (cdr results2)))
	  (else #f)))

  (define (rawmatch1 t1 t2)
    (fluid-let ((exact #f)
		(all #f))
      (match1 t1 t2)))

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
	  ((eq? t2 '*) (and (not exact) (not all)))
	  ((eq? t1 'undefined) #f)
	  ((eq? t2 'undefined) #f)
	  ((and (pair? t1) (eq? 'not (car t1)))
	   (fluid-let ((exact #f)
		       (all #f))
	     (let* ((trail0 trail)
		    (m (match1 (cadr t1) t2)))
	       (trail-restore trail0 typeenv)
	       (not m))))
	  ((and (pair? t2) (eq? 'not (car t2)))
	   (and (not exact)
		(let* ((trail0 trail)
		       (m (match1 t1 (cadr t2))))
		  (trail-restore trail0 typeenv)
		  (not m))))
	  ;; this is subtle: "or" types for t2 are less restrictive,
	  ;; so we handle them before "or" types for t1
	  ((and (pair? t2) (eq? 'or (car t2)))
	   (over-all-instantiations
	    (cdr t2)
	    typeenv 
	    (or exact all)
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
	  ((eq? t1 'noreturn) (not exact))
	  ((eq? t2 'noreturn) (not exact))
	  ((eq? t1 'number) 
	   (and (not exact)
		(match1 '(or fixnum float) t2)))
	  ((eq? t2 'number)
	   (and (not exact)
		(match1 t1 '(or fixnum float))))
	  ((eq? 'procedure t1)
	   (and (pair? t2)
		(eq? 'procedure (car t2))))
	  ((eq? 'procedure t2) 
	   (and (not exact)
		(pair? t1)
		(eq? 'procedure (car t1))))
	  ((eq? t1 'pair) (match1 '(pair * *) t2))
	  ((eq? t2 'pair) (match1 t1 '(pair * *)))
	  ((eq? t1 'list) (match1 '(list-of *) t2))
	  ((eq? t2 'list) (match1 t1 '(list-of *)))
	  ((eq? t1 'vector) (match1 '(vector-of *) t2))
	  ((eq? t2 'vector) (match1 t1 '(vector-of *)))
	  ((eq? t1 'null)
	   (and (not exact) (not all)
		(or (memq t2 '(null list))
		    (and (pair? t2) (eq? 'list-of (car t2))))))
	  ((eq? t2 'null)
	   (and (not exact)
		(or (memq t1 '(null list))
		    (and (pair? t1) (eq? 'list-of (car t1))))))
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
	     ((pair) (every match1 (cdr t1) (cdr t2)))
	     ((list-of vector-of) (match1 (second t1) (second t2)))
	     ((list vector)
	      (and (= (length t1) (length t2))
		   (every match1 (cdr t1) (cdr t2))))
	     (else #f) ) )
	  ((and (pair? t1) (eq? 'pair (car t1)))
	   (and (not exact) (not all)
		(pair? t2)
		(case (car t2)
		  ((list-of)
		   (let ((ct1 (canonicalize-list-type t1)))
		     (if ct1
			 (match1 ct1 t2)
			 #t)))		; inexact match
		  ((list)
		   (and (match1 (second t1) (second t2))
			(match1 (third t1)
				(if (null? (cdr t2))
				    'null
				    `(list ,@(cddr t2))))))
		  (else #f))))
	  ((and (pair? t2) (eq? 'pair (car t2)))
	   (and (pair? t1)
		(case (car t1)
		  ((list-of)
		   (let ((ct2 (canonicalize-list-type t2)))
		     (if ct2
			 (match1 t1 ct2)
			 (and (not exact) (not all)))))	; inexact mode: ok
		  ((list)
		   (and (match1 (second t1) (second t2))
			(match1 (if (null? (cdr t1))
				    'null
				    `(list ,@(cddr t1)))
				(third t2))))
		  (else #f))))
	  ((and (pair? t1) (eq? 'list-of (car t1)))
	   (or (eq? 'null t2)
	       (and (pair? t2)
		    (case (car t2)
		      ((list)
		       (let ((t1 (second t1)))
			 (over-all-instantiations
			  (cdr t2)
			  typeenv
			  #t
			  (lambda (t) (match1 t1 t)))))
		      ((pair)
		       (let ((ct2 (canonicalize-list-type t2)))
			 (and ct2 (match1 t1 ct2))))
		      (else #f)))))
	  ((and (pair? t1) (eq? 'list (car t1)))
	   (and (pair? t2)
		(case (car t2)
		  ((pair)
		   (and (pair? (cdr t1))
			(match1 (second t1) (second t2))
			(match1 t1 (third t2))))
		  ((list-of)
		   (and (not exact) 
			(not all)
			(let ((t2 (second t2)))
			  (over-all-instantiations
			   (cdr t1)
			   typeenv 
			   #t
			   (lambda (t) (match1 t t2))))))
		  (else #f))))
	  ((and (pair? t2) (eq? 'list-of (car t2)))
	   (and (not exact)		;XXX also check "all"?
		(or (eq? 'null t1)
		    (and (pair? t1)
			 (eq? 'pair (car t1)) ; list-of already handled above
			 (let ((ct1 (canonicalize-list-type t1)))
			   (and ct1 (match1 ct1 t2)))))))
	  ((and (pair? t2) (eq? 'list (car t2)))
	   (and (pair? t1)
		(case (car t1)
		  ((pair)
		   (and (pair? (cdr t2))
			(match1 (second t1) (second t2))
			(match1 (third t1) t2)))
		  ;; t1 = list-of already handled above
		  (else #f))))
	  ((and (pair? t1) (eq? 'vector (car t1)))
	   (and (not exact) (not all)
		(pair? t2)
		(eq? 'vector-of (car t2))
		(let ((t2 (second t2)))
		  (over-all-instantiations
		   (cdr t1)
		   typeenv
		   #t
		   (lambda (t) (match1 t t2))))))
	  ((and (pair? t2) (eq? 'vector (car t2)))
	   (and (pair? t1)
		(eq? 'vector-of (car t1))
		(let ((t1 (second t1)))
		  (over-all-instantiations
		   (cdr t2)
		   typeenv 
		   #t
		   (lambda (t) (match1 t1 t))))))
	  (else #f)))

  (let ((m (match1 t1 t2)))
    (dd "    match~a~a ~a <-> ~a -> ~a  te: ~s" 
	(if exact " (exact)" "") 
	(if all " (all)" "") 
	t1 t2 m typeenv)
    m))


(define (match-argument-types typelist atypes typeenv #!optional exact all)
  ;; this doesn't need optional: it is only used for predicate- and specialization
  ;; matching
  (let loop ((tl typelist) (atypes atypes))
    (cond ((null? tl) (null? atypes))
	  ((null? atypes) #f)
	  ((equal? '(#!rest) tl))
	  ((eq? (car tl) '#!rest)
	   (every 
	    (lambda (at)
	      (match-types (cadr tl) at typeenv exact all))
	    atypes))
	  ((match-types (car tl) (car atypes) typeenv exact all)
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
    (define (rename v)
      (cond ((assq v typeenv) => cdr)
	    (else
	     (let ((new (gensym v)))
	       (set! typeenv (alist-cons v new typeenv))
	       new))))
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
					(cons v (gensym v))) )
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
 		   (let* ((ts (map simplify (cdr t)))
 			  (tslen (length ts)))
 		     (cond ((= 1 tslen) (car ts))
			   ((null? ts) '*)
 			   ((> tslen +maximal-union-type-length+)
 			    (d "union-type cutoff! (~a): ~s" tslen ts)
 			    '*)
			   ((every procedure-type? ts)
			    (if (any (cut eq? 'procedure <>) ts)
				'procedure
				(reduce
				 (lambda (t pt)
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
				 #f
				 ts)))
			   ((lset= eq? '(fixnum float) ts) 'number)
			   (else
			    (let* ((ts (append-map
					(lambda (t)
					  (let ((t (simplify t)))
					    (cond ((and (pair? t) (eq? 'or (car t)))
						   (cdr t))
						  ((eq? t 'undefined) (return 'undefined))
						  ((eq? t 'noreturn) '())
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
			      (cond ((equal? ts2 (cdr t)) t)
				    (else
				     (dd "  or-simplify: ~a" ts2)
				     (simplify 
				      `(or ,@(if (any (cut eq? <> '*) ts2) '(*) ts2)))))))) ))
		  ((pair) 
		   (let ((tcar (simplify (second t)))
			 (tcdr (simplify (third t))))
		     (if (and (eq? '* tcar) (eq? '* tcdr))
			 'pair
			 (let ((t `(pair ,tcar ,tcdr)))
			   (or (canonicalize-list-type t)
			       t)))))
		  ((vector-of)
		   (let ((t2 (simplify (second t))))
		     (if (eq? t2 '*)
			 'vector
			 `(,(car t) ,t2))))
		  ((vector-of list-of)
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
		  (set! used (lset-adjoin eq? used t))
		  (cdr e)))
	       (else t)))))
    (let ((t2 (simplify t)))
      (when (pair? typeenv)
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
       (cond ((null? ts1) '())
	     ((null? ts2) '())
	     ((or (atom? ts1) (atom? ts2)) (return '*))
	     ((eq? 'noreturn (car ts1)) (loop (cdr ts1) ts2))
	     ((eq? 'noreturn (car ts2)) (loop ts1 (cdr ts2)))
	     (else (cons (simplify-type `(or ,(car ts1) ,(car ts2)))
			 (loop (cdr ts1) (cdr ts2)))))))))


(define (compatible-types? t1 t2)
  (or (type<=? t1 t2)
      (type<=? t2 t1)))


(define (type<=? t1 t2)
  (let* ((typeenv (append-map type-typeenv (list t1 t2)))
	 (trail0 trail)
	 (r (let test ((t1 t1) (t2 t2))
	      (cond ((eq? t1 t2))
		    ((and (symbol? t1) (assq t1 typeenv)) =>
		     (lambda (e)
		       (cond ((second e) (test (second e) t2))
			     (else 
			      (set-car! (cdr e) t2)
			      (or (not (third e))
				  (test (third e) t2))))))
		    ((and (symbol? t2) (assq t2 typeenv)) =>
		     (lambda (e) 
		       (cond ((second e) (test t1 (second e)))
			     (else
			      (set-cdr! e t1)
			      (or (not (third e))
				  (test t1 (third e)))))))
		    ((memq t2 '(* undefined)))
		    ((eq? 'pair t1) (test '(pair * *) t2))
		    ((eq? 'vector t1) (test '(vector-of *) t2))
		    ((eq? 'list t1) (test '(list-of *) t2))
		    ((and (eq? 'null t1)
			  (pair? t2) 
			  (eq? (car t2) 'list-of)))
		    ((and (pair? t1) (eq? 'forall (car t1)))
		     (test (third t1) t2))
		    ((and (pair? t2) (eq? 'forall (car t2)))
		     (test t1 (third t2)))
		    (else
		     (case t2
		       ((procedure) (and (pair? t1) (eq? 'procedure (car t1))))
		       ((number) (memq t1 '(fixnum float)))
		       ((vector) (test t1 '(vector-of *)))
		       ((list) (test t1 '(list-of *)))
		       ((pair) (test t1 '(pair * *)))
		       (else
			(cond ((not (pair? t1)) #f)
			      ((not (pair? t2)) #f)
			      ((eq? 'or (car t2))
			       (over-all-instantiations
				(cdr t2)
				typeenv
				#t
				(lambda (t) (test t1 t))))
			      ((and (eq? 'vector (car t1)) (eq? 'vector-of (car t2)))
			       (every (cute test <> (second t2)) (cdr t1)))
			      ((and (eq? 'vector-of (car t1)) (eq? 'vector (car t2)))
			       (every (cute test (second t1) <>) (cdr t2)))
			      ((and (eq? 'list (car t1)) (eq? 'list-of (car t2)))
			       (every (cute test <> (second t2)) (cdr t1)))
			      ((and (eq? 'list (car t1)) (eq? 'pair (car t2)))
			       (and (not (null? (cdr t1)))
				    (test (second t1) (second t2))
				    (test t1 (third t2))))
			      ((and (eq? 'pair (car t1)) (eq? 'list (car t2)))
			       (and (not (null? (cdr t2)))
				    (test (second t1) (second t2))
				    (test (third t1) t2)))
			      ((and (eq? 'pair (car t1)) (eq? 'list-of (car t2)))
			       (and (test (second t1) (second t2))
				    (test (third t1) t2)))
			      ((not (eq? (car t1) (car t2))) #f)
			      (else
			       (case (car t1)
				 ((or) 
				  (over-all-instantiations
				   (cdr t1)
				   typeenv
				   #t
				   (lambda (t) (test t t2))))
				 ((vector-of list-of) (test (second t1) (second t2)))
				 ((pair) (every test (cdr t1) (cdr t2)))
				 ((list vector)
				  (and (= (length t1) (length t2))
				       (every test (cdr t1) (cdr t2))))
				 ((struct) (eq? (cadr t1) (cadr t2)))
				 ((procedure)
				  (let ((args1 (if (named? t1) (caddr t1) (cadr t1)))
					(args2 (if (named? t2) (caddr t2) (cadr t2)))
					(res1 (if (named? t1) (cdddr t1) (cddr t1)))
					(res2 (if (named? t2) (cdddr t2) (cddr t2))) )
				    (let loop1 ((args1 args1)
						(args2 args2)
						(rtype1 #f)
						(rtype2 #f)
						(m1 0) 
						(m2 0))
				      (cond ((null? args1)
					     (and (cond ((null? args2)
							 (if rtype1
							     (if rtype2
								 (test rtype1 rtype2)
								 #f)
							     #t))
							((eq? '#!optional (car args2))
							 (not rtype1))
							((eq? '#!rest (car args2))
							 (or (null? (cdr args2))
							     rtype1
							     (test rtype1 (cadr args2))))
							(else (>= m2 m1)))
						  (let loop2 ((res1 res1) (res2 res2))
						    (cond ((eq? '* res2) #t)
							  ((null? res2) (null? res1))
							  ((eq? '* res1) #f)
							  ((test (car res1) (car res2))
							   (loop2 (cdr res1) (cdr res2)))
							  (else #f)))))
					    ((eq? (car args1) '#!optional)
					     (loop1 (cdr args1) args2 #f rtype2 1 m2))
					    ((eq? (car args1) '#!rest)
					     (if (null? (cdr args1))
						 (loop1 '() args2 '* rtype2 2 m2)
						 (loop1 '() args2 (cadr args1) rtype2 2 m2)))
					    ((null? args2) 
					     (and rtype2
						  (test (car args1) rtype2)
						  (loop1 (cdr args1) '() rtype1 rtype2 m1 m2)))
					    ((eq? (car args2) '#!optional)
					     (loop1 args1 (cdr args2) rtype1 #f m1 1))
					    ((eq? (car args2) '#!rest)
					     (if (null? (cdr args2))
						 (loop1 args1 '() rtype1 '* m1 2)
						 (loop1 args1 '() rtype1 (cadr args2) m1 2)))
					    ((test
					      (or rtype1 (car args1))
					      (or rtype2 (car args2)))
					     (loop1 (cdr args1) (cdr args2) rtype1 rtype2 m1 m2))
					    (else #f)))))
				 (else #f)))))))))))
    (set! trail trail0)
    ;;(dd "type<=?: ~s <-> ~s -> ~s" t1 t2 r)
    r))


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


;;; Type-environments and -variables

(define (type-typeenv t)
  (let ((te '()))
    (let loop ((t t))
      (when (pair? t)
	(case (car t)
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
	  ((or and)
	   (for-each loop (cdr t))))))
    te))

(define (trail-restore tr typeenv)
  (do ((tr2 trail (cdr tr2)))
      ((eq? tr2 tr))
    (let ((a (assq (car tr2) typeenv)))
      (set-car! (cdr a) #f))))

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
	    (if (memq t '(* fixnum eof char string symbol float number list vector pair
			    undefined blob input-port output-port pointer locative boolean 
			    pointer-vector null procedure noreturn))
		t
		(bomb "resolve: can't resolve unknown type-variable" t)))
	   (else 
	    (case (car t)
	      ((or) `(or ,@(map (cut resolve <> done) (cdr t))))
	      ((not) `(not ,(resolve (second t) done)))
	      ((forall) `(forall ,(second t) ,(resolve (third t) done)))
	      ((pair list vector vector-of list-of) 
	       (cons (car t) (map (cut resolve <> done) (cdr t))))
	      ((procedure)
	       (let* ((argtypes (procedure-arguments t))
		      (rtypes (procedure-results t)))
		 `(procedure
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

(define (load-type-database name #!optional (path (repository-path)))
  (define (clean! name)
    (when enable-specialization 
      (mark-variable name '##compiler#clean #t)))
  (define (pure! name)
    (when enable-specialization 
      (mark-variable name '##compiler#pure #t)))
  (and-let* ((dbfile (file-exists? (make-pathname path name))))
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
	       (warning "invalid type specification" name new))
	     (when (and old (not (compatible-types? old t)))
	       (warning
		(sprintf
		    "type-definition `~a' for toplevel binding `~a' conflicts with previously loaded type `~a'"
		  name new old)))
	     (mark-variable name '##compiler#type t)
	     (when specs
	       (install-specializations name specs)))))
       (read-file dbfile))
      #t)))

(define (emit-type-file filename db)
  (with-output-to-file filename
    (lambda ()
      (print "; GENERATED BY CHICKEN " (chicken-version) " FROM "
	     source-filename "\n")
      (##sys#hash-table-for-each
       (lambda (sym plist)
	 (when (and (variable-visible? sym)
		    (variable-mark sym '##compiler#declared-type))
	   (let ((specs (or (variable-mark sym '##compiler#specializations) '()))
		 (type (variable-mark sym '##compiler#type))
		 (pred (variable-mark sym '##compiler#predicate))
		 (pure (variable-mark sym '##compiler#pure))
		 (clean (variable-mark sym '##compiler#clean))
		 (enforce (variable-mark sym '##compiler#enforce)))
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
			       ,@(if clean '(#:clean) '()))
			     ,@(cdr type)))
			  ((forall)
			   `(forall ,(second type) ,(wrap (third type))))
			  (else type))
			type))
		  specs))
	     (newline))))
       db)
      (print "; END OF FILE"))))


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
    (let ((spec (subst template)))
      (copy-node! (build-node-graph spec) node))))


;;; Type-validation and -normalization

(define (validate-type type name)
  ;; - returns converted type or #f
  ;; - also converts "(... -> ...)" types
  ;; - converts some typenames to struct types (u32vector, etc.)
  ;; - handles some type aliases
  ;; - drops "#!key ..." args by converting to #!rest
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
    (define (validate-llist llist)
      (cond ((null? llist) '())
	    ((symbol? llist) '(#!rest *))
	    ((not (pair? llist)) #f)
	    ((eq? '#!optional (car llist))
	     (let ((l1 (validate-llist (cdr llist))))
	       (and l1 (cons '#!optional l1))))
	    ((eq? '#!rest (car llist))
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
      (cond ((memq t '(* string symbol char number boolean list pair
			 procedure vector null eof undefined input-port output-port
			 blob pointer locative fixnum float pointer-vector
			 deprecated noreturn values))
	     t)
	    ((memq t '(u8vector s8vector u16vector s16vector u32vector s32vector
				f32vector f64vector thread queue environment time
				continuation lock mmap condition hash-table
				tcp-listener))
	     `(struct ,t))
	    ((eq? t 'immediate)
	     '(or eof null fixnum char boolean))
	    ((eq? t 'port)
	     '(or input-port output-port))
	    ((eq? t 'any) '*)
	    ((eq? t 'void) 'undefined)
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
	    ((eq? 'or (car t)) 
	     (and (list? t)
		  (let ((ts (map validate (cdr t))))
		    (and (every identity ts)
			 `(or ,@ts)))))
	    ((eq? 'struct (car t))
	     (and (= 2 (length t))
		  (symbol? (cadr t))
		  t))
	    ((eq? 'deprecated (car t))
	     (and (= 2 (length t)) (symbol? (second t)) t))
	    ((or (memq '--> t) (memq '-> t)) =>
	     (lambda (p)
	       (let* ((cleanf (eq? '--> (car p)))
		      (ok (or (not rec) (not cleanf))))
		 (unless rec (set! clean cleanf))
		 (let ((cp (memq ': (cdr p))))
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
  (let-values (((t pred pure) (validate-type (##sys#strip-syntax type) name)))
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
; - returns #f if not possibly matchable with "list-of"

(define (canonicalize-list-type t)
  (cond ((not (pair? t)) t)
	((eq? 'pair (car t))
	 (let ((tcar (second t))
	       (tcdr (third t)))
	   (let rec ((tr tcdr) (ts (list tcar)))
	     (cond ((eq? 'null tr)
		    `(list ,@(reverse ts)))
		   ((eq? 'list tr) tr)
		   ((and (pair? tr) (eq? 'pair (first tr)))
		    (rec (third tr) (cons (second tr) ts)))
		   ((and (pair? tr) (eq? 'list (first tr)))
		    `(list ,@(reverse ts) ,@(cdr tr)))
		   ((and (pair? tr) (eq? 'list-of (first tr)))
		    `(list-of
		      ,(simplify-type
			`(or ,@(reverse ts) ,@(cdr tr)))))
		   (else #f)))))
	(else t)))


;;; hardcoded result types for certain primitives

(define-syntax define-special-case
  (syntax-rules ()
    ((_ name handler)
     (##sys#put! 'name '##compiler#special-result-type handler))))

(define-special-case ##sys#make-structure
  (lambda (node args rtypes)
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
              `((struct ,val))))
	rtypes)))

(let ()
  (define (vector-ref-result-type node args rtypes)
    (or (and-let* ((subs (node-subexpressions node))
                   ((= (length subs) 3))
                   (arg1 (walked-result (second args)))
                   ((pair? arg1))
                   ((eq? 'vector (car arg1)))
                   (index (third subs))
                   ((eq? 'quote (node-class index)))
                   (val (first (node-parameters index)))
                   ((fixnum? val))
                   ((>= val 0))
                   ((< val (length (cdr arg1))))) ;XXX could warn on failure (but needs location)
          (list (list-ref (cdr arg1) val)))
	rtypes))
  (define-special-case vector-ref vector-ref-result-type)
  (define-special-case ##sys#vector-ref vector-ref-result-type))

(let ()
  (define (list-ref-result-type node args rtypes)
    (or (and-let* ((subs (node-subexpressions node))
                   ((= (length subs) 3))
                   (arg1 (walked-result (second args)))
                   ((pair? arg1))
                   ((eq? 'list (car arg1)))
                   (index (third subs))
                   ((eq? 'quote (node-class index)))
                   (val (first (node-parameters index)))
                   ((fixnum? val))
                   ((>= val 0))  ;XXX could warn on failure (but needs location)
                   ((< val (length (cdr arg1)))))
          (list (list-ref (cdr arg1) val)))
	rtypes))
  (define-special-case list-ref list-ref-result-type)
  (define-special-case ##sys#list-ref list-ref-result-type))

(define-special-case list-tail
  (lambda (node args rtypes)
    (or (and-let* ((subs (node-subexpressions node))
                   ((= (length subs) 3))
                   (arg1 (walked-result (second args)))
                   ((pair? arg1))
                   ((eq? 'list (car arg1)))
                   (index (third subs))
                   ((eq? 'quote (node-class index)))
                   (val (first (node-parameters index)))
                   ((fixnum? val))
                   ((>= val 0))
                   ((< val (length (cdr arg1)))))   ;XXX could warn on failure (but needs location)
          (let ((rest (list-tail (cdr arg1) val)))
            (list (if (null? rest)
                      'null
                      `(list ,@rest)))))
	rtypes)))

(define-special-case list
  (lambda (node args rtypes)
    (if (null? (cdr args))
	'(null)
	`((list ,@(map walked-result (cdr args)))))))

(define-special-case ##sys#list
  (lambda (node args rtypes)
    (if (null? (cdr args))
	'(null)
	`((list ,@(map walked-result (cdr args)))))))

(define-special-case vector
  (lambda (node args rtypes)
    `((vector ,@(map walked-result (cdr args))))))

(define-special-case ##sys#vector
  (lambda (node args rtypes)
    `((vector ,@(map walked-result (cdr args))))))


;;; perform check over all typevar instantiations

(define (over-all-instantiations tlist typeenv exact process)
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
				   (exact '*)
				   (else #f)))
			   insts)))
		       vars)))
	(ddd "  collected: ~s" all)
	all))

    (ddd " over-all-instantiations: ~s exact=~a" tlist exact)
    ;; process all tlist elements
    (let loop ((ts tlist) (ok #f))
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
	    (exact 
	     (restore)
	     #f)
	    (else 
	     (restore)
	     (loop (cdr ts) ok))))))
