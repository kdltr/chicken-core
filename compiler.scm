;;;; compiler.scm - The CHICKEN Scheme compiler
;
;
; "This is insane. What we clearly want to do is not exactly clear, and is rooted in NCOMPLR."
;
;
;--------------------------------------------------------------------------------------------
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
;
;
; Supported syntax:
;
; - Declaration specifiers:
;
; ([not] extended-bindings {<name>})
; ([not] inline {<var>})
; ([not] interrupts-enabled)
; ([not] safe)
; ([not] standard-bindings {<name>})
; ([not] usual-integrations {<name>})
; (local {<name> ...})
; ([not] inline-global {<name>})
; ([number-type] <type>)
; (always-bound {<name>})
; (block)
; (block-global {<name>})
; (bound-to-procedure {<var>})
; (c-options {<opt>})
; (compile-syntax)
; (disable-interrupts)
; (emit-import-library {<module> | (<module> <filename>)})
; (export {<name>})
; (fixnum-arithmetic)
; (foreign-declare {<string>})
; (hide {<name>})
; (inline-limit <limit>)
; (keep-shadowed-macros)
; (no-argc-checks)
; (no-bound-checks)
; (no-procedure-checks)
; (no-procedure-checks-for-usual-bindings)
; (no-procedure-checks-for-toplevel-bindings)
; (profile <symbol> ...)
; (safe-globals)
; (separate)
; (type (<symbol> <typespec>) ...)
; (unit <unitname>)
; (unsafe)
; (unused <symbol> ...)
; (uses {<unitname>})
; (strict-types)
; (specialize)
; (enforce-argument-types [<symbol> ...])
;
;   <type> = fixnum | generic

; - Global symbol properties:
;
;   ##compiler#always-bound -> BOOL
;   ##compiler#always-bound-to-procedure -> BOOL
;   ##compiler#local -> BOOL
;   ##compiler#visibility -> #f | 'hidden | 'exported
;   ##compiler#constant -> BOOL                             defined as constant
;   ##compiler#intrinsic -> #f | 'standard | 'extended
;   ##compiler#inline -> 'no | 'yes
;   ##compiler#inline-global -> 'yes | 'no | <node>
;   ##compiler#profile -> BOOL
;   ##compiler#unused -> BOOL
;   ##compiler#foldable -> BOOL
;   ##compiler#pure -> BOOL                                 referentially transparent
;   ##compiler#clean -> BOOL                                does not modify local state
;   ##compiler#type -> TYPE
;   ##compiler#declared-type -> BOOL

; - Source language:
;
; <variable>
; <constant>
; (##core#declare {<spec>})
; (##core#immutable <exp>)
; (##core#quote <exp>)
; (##core#syntax <exp>)
; (##core#if <exp> <exp> [<exp>])
; (##core#let <variable> ({(<variable> <exp>)}) <body>)
; (##core#let ({(<variable> <exp>)}) <body>)
; (##core#letrec ({(<variable> <exp>)}) <body>)
; (##core#let-location <symbol> <type> [<init>] <exp>)
; (##core#lambda <variable> <body>)
; (##core#lambda ({<variable>}+ [. <variable>]) <body>)
; (##core#set! <variable> <exp>)
; (##core#begin <exp> ...)
; (##core#toplevel-begin <exp> ...)
; (##core#include <string>)
; (##core#loop-lambda <llist> <body>)
; (##core#undefined)
; (##core#primitive <name>)
; (##core#inline {<op>} <exp>)
; (##core#inline_allocate (<op> <words>) {<exp>})
; (##core#inline_ref (<name> <type>))
; (##core#inline_update (<name> <type>) <exp>)
; (##core#inline_loc_ref (<type>) <exp>)
; (##core#inline_loc_update (<type>) <exp> <exp>)
; (##core#compiletimetoo <exp>)
; (##core#compiletimeonly <exp>)
; (##core#elaborationtimetoo <exp>)
; (##core#elaborationtimeonly <exp>)
; (##core#define-foreign-variable <symbol> <type> [<string>])
; (##core#define-foreign-type <symbol> <type> [<proc1> [<proc2>]])
; (##core#foreign-lambda <type> <string> {<type>})
; (##core#foreign-lambda* <type> ({(<type> <var>)})) {<string>})
; (##core#foreign-safe-lambda <type> <string> {<type>})
; (##core#foreign-safe-lambda* <type> ({(<type> <var>)})) {<string>})
; (##core#foreign-primitive <type> ({(<type> <var>)}) {<string>})
; (##core#define-inline <name> <exp>)
; (##core#define-constant <name> <exp*>)
; (##core#foreign-callback-wrapper '<name> <qualifiers> '<type> '({<type>}) <exp>)
; (##core#define-external-variable <name> <type> <bool> [<symbol>])
; (##core#check <exp>)
; (##core#require-for-syntax <exp> ...)
; (##core#require-extension (<id> ...) <bool>)
; (##core#app <exp> {<exp>})
; (##core#define-syntax <symbol> <expr>)
; (##core#define-compiler-syntax <symbol> <expr>)
; (##core#let-compiler-syntax ((<symbol> <expr>) ...) <expr> ...)
; (##core#module <symbol> #t | (<name> | (<name> ...) ...) <body>)
; (##core#let-module-alias ((<alias> <name>) ...) <body>)
; (##core#the <type> <strict?> <exp>)
; (##core#typecase <info> <exp> (<type> <body>) ... [(else <body>)])
; (<exp> {<exp>})

; - Core language:
;
; [##core#variable {<variable>}]
; [if {} <exp> <exp> <exp>)]
; [quote {<exp>}]
; [let {<variable>} <exp-v> <exp>]
; [##core#lambda {<id> <mode> (<variable>... [. <variable>]) <size>} <exp>]
; [set! {<variable>} <exp>]
; [##core#undefined {}]
; [##core#primitive {<name>}]
; [##core#inline {<op>} <exp>...]
; [##core#inline_allocate {<op> <words>} <exp>...]
; [##core#inline_ref {<name> <type>}]
; [##core#inline_update {<name> <type>} <exp>]
; [##core#inline_loc_ref {<type>} <exp>]
; [##core#inline_loc_update {<type>} <exp> <exp>]
; [##core#call {<safe-flag> [<debug-info>]} <exp-f> <exp>...]
; [##core#callunit {<unitname>} <exp>...]
; [##core#switch {<count>} <exp> <const1> <body1> ... <defaultbody>]
; [##core#cond <exp> <exp> <exp>]
; [##core#recurse {<tail-flag>} <exp1> ...]
; [##core#return <exp>]
; [##core#direct_call {<safe-flag> <debug-info> <call-id> <words>} <exp-f> <exp>...]
; [##core#direct_lambda {<id> <mode> (<variable>... [. <variable>]) <size>} <exp>]
; [##core#the {<type> <strict>} <exp>]
; [##core#the/result {<typelist>} <exp>]
; [##core#typecase {<info> (<type> ...)} <exp> <body1> ... [<elsebody>]]

; - Closure converted/prepared language:
;
; [if {} <exp> <exp> <exp>]
; [quote {<exp>}]
; [##core#bind {<count>} <exp-v>... <exp>]
; [##core#let_unboxed {<name> <utype>} <exp1> <exp2>]
; [##core#undefined {}]
; [##core#unboxed_ref {<name> [<utype>]}]
; [##core#unboxed_set! {<name> <utype>} <exp>]
; [##core#inline {<op>} <exp>...]
; [##core#inline_allocate {<op <words>} <exp>...]
; [##core#inline_ref {<name> <type>}]
; [##core#inline_update {<name> <type>} <exp>]
; [##core#inline_loc_ref {<type>} <exp>]
; [##core#inline_loc_update {<type>} <exp> <exp>]
; [##core#inline_unboxed {<op>} <exp> ...]
; [##core#closure {<count>} <exp>...]
; [##core#box {} <exp>]
; [##core#unbox {} <exp>]
; [##core#ref {<index>} <exp>]
; [##core#update {<index>} <exp> <exp>]
; [##core#updatebox {} <exp> <exp>]
; [##core#update_i {<index>} <exp> <exp>]
; [##core#updatebox_i {} <exp> <exp>]
; [##core#call {<safe-flag> [<debug-info> [<call-id> <customizable-flag>]]} <exp-f> <exp>...]
; [##core#callunit {<unitname>} <exp>...]
; [##core#cond <exp> <exp> <exp>]
; [##core#local {<index>}]
; [##core#setlocal {<index>} <exp>]
; [##core#global {<literal> <safe-flag> <block-mode> [<name>]}]
; [##core#setglobal {<literal> <block-mode> <name>} <exp>]
; [##core#setglobal_i {<literal> <block-mode> <name>} <exp>]
; [##core#literal {<literal>}]
; [##core#immediate {<type> [<immediate>]}]     - type: bool/fix/nil/char
; [##core#proc {<name> [<non-internal>]}]
; [##core#recurse {<tail-flag> <call-id>} <exp1> ...]
; [##core#return <exp>]
; [##core#direct_call {<safe-flag> <debug-info> <call-id> <words>} <exp-f> <exp>...]

; Analysis database entries:
;
; <variable>:
;
;   captured -> <boolean>                    If true: variable is used outside it's home-scope
;   global -> <boolean>                      If true: variable does not occur in any lambda-list
;   call-sites -> ((<lambda-id> <node>) ...) Known call-nodes of a named procedure
;   home -> <lambda-id>                      Procedure which introduces this variable
;   unknown -> <boolean>                     If true: variable cannot have a known value
;   assigned -> <boolean>                    If true: variable is assigned somewhere
;   assigned-locally -> <boolean>            If true: variable has been assigned inside user lambda
;   undefined -> <boolean>                   If true: variable is unknown yet but can be known later
;   value -> <node>                          Variable has a known value
;   local-value -> <node>                    Variable is declared local and has value
;   potential-value -> <node>                Global variable was assigned this value (used for lambda-info)
;   references -> (<node> ...)               Nodes that are accesses of this variable (##core#variable nodes)
;   boxed -> <boolean>                       If true: variable has to be boxed after closure-conversion
;   contractable -> <boolean>                If true: variable names contractable procedure
;   inlinable -> <boolean>                   If true: variable names potentially inlinable procedure
;   collapsable -> <boolean>                 If true: variable refers to collapsable constant
;   removable -> <boolean>                   If true: variable is not used
;   replacable -> <variable>                 Variable can be replaced by another variable
;   replacing -> <boolean>                   If true: variable can replace another variable (don't remove)
;   standard-binding -> <boolean>            If true: variable names a standard binding
;   extended-binding -> <boolean>            If true: variable names an extended binding
;   unused -> <boolean>                      If true: variable is a formal parameter that is never used
;   rest-parameter -> #f | 'list             If true: variable holds rest-argument list
;   constant -> <boolean>                    If true: variable has fixed value
;   hidden-refs -> <boolean>                 If true: procedure that refers to hidden global variables
;   inline-transient -> <boolean>            If true: was introduced during inlining
; 
; <lambda-id>:
;
;   contains -> (<lambda-id> ...)            Procedures contained in this lambda
;   contained-in -> <lambda-id>              Procedure containing this lambda
;   has-unused-parameters -> <boolean>       If true: procedure has unused formal parameters
;   use-expr -> (<lambda-id> ...)            Marks non-direct use-sites of common subexpression
;   closure-size -> <integer>                Number of free variables stored in a closure
;   customizable -> <boolean>                If true: all call sites are known, procedure does not escape
;   simple -> <boolean>                      If true: procedure only calls its continuation
;   explicit-rest -> <boolean>               If true: procedure is called with consed rest list
;   captured-variables -> (<var> ...)        List of closed over variables
;   inline-target -> <boolean>               If true: was target of an inlining operation


(declare
 (unit compiler))


(include "compiler-namespace")

(define (d arg1 . more)
  (if (null? more)
      (pp arg1)
      (apply print arg1 more)))

(define-syntax d (syntax-rules () ((_ . _) (void))))

(include "tweaks")


(define-inline (gensym-f-id) (gensym 'f_))

(define-foreign-variable installation-home c-string "C_INSTALL_SHARE_HOME")
(define-foreign-variable default-target-heap-size int "C_DEFAULT_TARGET_HEAP_SIZE")

(define-constant foreign-type-table-size 301)
(define-constant initial-analysis-database-size 3001)
(define-constant default-line-number-database-size 997)
(define-constant inline-table-size 301)
(define-constant constant-table-size 301)
(define-constant file-requirements-size 301)
(define-constant real-name-table-size 997)
(define-constant default-inline-max-size 20)


;;; Global variables containing compilation parameters:

(define unit-name #f)
(define number-type 'generic)
(define standard-bindings '())
(define extended-bindings '())
(define insert-timer-checks #t)
(define used-units '())
(define unsafe #f)
(define foreign-declarations '())
(define emit-trace-info #f)
(define block-compilation #f)
(define line-number-database-size default-line-number-database-size)
(define target-heap-size #f)
(define target-stack-size #f)
(define optimize-leaf-routines #f)
(define emit-profile #f)
(define no-bound-checks #f)
(define no-argc-checks #f)
(define no-procedure-checks #f)
(define no-global-procedure-checks #f)
(define source-filename #f)
(define safe-globals-flag #f)
(define explicit-use-flag #f)
(define disable-stack-overflow-checking #f)
(define require-imports-flag #f)
(define external-protos-first #f)
(define inline-max-size default-inline-max-size)
(define emit-closure-info #t)
(define undefine-shadowed-macros #t)
(define profiled-procedures #f)
(define import-libraries '())
(define all-import-libraries #f)
(define enable-module-registration #t)
(define standalone-executable #t)
(define local-definitions #f)
(define inline-locally #f)
(define enable-inline-files #f)
(define compiler-syntax-enabled #t)
(define bootstrap-mode #f)
(define strict-variable-types #f)
(define enable-specialization #f)

;;; Other global variables:

(define verbose-mode #f)
(define original-program-size #f)
(define current-program-size 0)
(define current-analysis-database-size initial-analysis-database-size)
(define line-number-database-2 #f)
(define immutable-constants '())
(define inline-table #f)
(define inline-table-used #f)
(define constant-table #f)
(define constants-used #f)
(define broken-constant-nodes '())
(define inline-substitutions-enabled #f)
(define direct-call-ids '())
(define first-analysis #t)
(define foreign-type-table #f)
(define foreign-variables '())
(define foreign-lambda-stubs '())
(define foreign-callback-stubs '())
(define external-variables '())
(define profile-lambda-list '())
(define profile-lambda-index 0)
(define profile-info-vector-name #f)
(define external-to-pointer '())
(define real-name-table #f)
(define location-pointer-map '())
(define pending-canonicalizations '())
(define defconstant-bindings '())
(define callback-names '())
(define toplevel-scope #t)
(define toplevel-lambda-id #f)
(define csc-control-file #f)
(define data-declarations '())
(define file-requirements #f)
(define postponed-initforms '())


;;; Initialize globals:

(define (initialize-compiler)
  (if line-number-database-2
      (vector-fill! line-number-database-2 '())
      (set! line-number-database-2 (make-vector line-number-database-size '())) )
  (if inline-table
      (vector-fill! inline-table '())
      (set! inline-table (make-vector inline-table-size '())) )
  (if constant-table
      (vector-fill! constant-table '())
      (set! constant-table (make-vector constant-table-size '())) )
  (set! profile-info-vector-name (make-random-name 'profile-info))
  (set! real-name-table (make-vector real-name-table-size '()))
  (if file-requirements
      (vector-fill! file-requirements '())
      (set! file-requirements (make-vector file-requirements-size '())) )
  (if foreign-type-table
      (vector-fill! foreign-type-table '())
      (set! foreign-type-table (make-vector foreign-type-table-size '())) ) )


;;; Expand macros and canonicalize expressions:

(define (canonicalize-expression exp)
  (let ((compiler-syntax '()))

  (define (find-id id se)		; ignores macro bindings
    (cond ((null? se) #f)
	  ((and (eq? id (caar se)) (symbol? (cdar se))) (cdar se))
	  (else (find-id id (cdr se)))))

  (define (lookup id se)
    (cond ((find-id id se))
	  ((##sys#get id '##core#macro-alias))
	  (else id)))

  (define (macro-alias var se)
    (let ((alias (gensym var)))
      (##sys#put! alias '##core#macro-alias (lookup var se))
      alias) )

  (define (set-real-names! as ns)
    (for-each (lambda (a n) (set-real-name! a n)) as ns) )

  (define (write-to-string x)
    (let ([out (open-output-string)])
      (write x out)
      (get-output-string out) ) )

  (define (unquotify x se)
    (if (and (list? x) 
	     (= 2 (length x))
	     (symbol? (car x))
	     (eq? 'quote (lookup (car x) se)))
	(cadr x)
	x) )

  (define (resolve-variable x0 e se dest ldest h)
    (let ((x (lookup x0 se)))
      (d `(RESOLVE-VARIABLE: ,x0 ,x ,(map (lambda (x) (car x)) se)))
      (cond ((not (symbol? x)) x0)	; syntax?
	    [(and constants-used (##sys#hash-table-ref constant-table x)) 
	     => (lambda (val) (walk (car val) e se dest ldest h #f)) ]
	    [(and inline-table-used (##sys#hash-table-ref inline-table x))
	     => (lambda (val) (walk val e se dest ldest h #f)) ]
	    [(assq x foreign-variables)
	     => (lambda (fv) 
		  (let* ([t (second fv)]
			 [ft (final-foreign-type t)] 
			 [body `(##core#inline_ref (,(third fv) ,t))] )
		    (walk
		     (foreign-type-convert-result
		      (finish-foreign-result ft body)
		      t)
		     e se dest ldest h #f)))]
	    [(assq x location-pointer-map)
	     => (lambda (a)
		  (let* ([t (third a)]
			 [ft (final-foreign-type t)] 
			 [body `(##core#inline_loc_ref (,t) ,(second a))] )
		    (walk
		     (foreign-type-convert-result
		      (finish-foreign-result ft body)
		      t)
		     e se dest ldest h #f))) ]
	    ((##sys#get x '##core#primitive))
	    ((not (memq x e)) (##sys#alias-global-hook x #f h)) ; only if global
	    (else x))))
  
  (define (emit-import-lib name il)
    (let* ((fname (if all-import-libraries
		      (string-append (symbol->string name) ".import.scm")
		      (cdr il)))
	   (imps (##sys#compiled-module-registration (##sys#current-module)))
	   (oldimps
	    (and (file-exists? fname)
		 (read-file fname) ) ) )
      (cond ((equal? imps oldimps)
	     (when verbose-mode
	       (print "not generating import library `" fname "' for module `" 
		      name "' because imports did not change")) )
	    (else
	     (when verbose-mode
	       (print "generating import library `" fname "' for module `"
		      name "' ..."))
	     (with-output-to-file fname
	       (lambda ()
		 (print ";;;; " fname " - GENERATED BY CHICKEN "
			(chicken-version) " -*- Scheme -*-\n")
		 (for-each pretty-print imps)
		 (print "\n;; END OF FILE"))))) ) )

  (define (walk x e se dest ldest h outer-ln)
    (cond ((symbol? x)
	   (cond ((keyword? x) `(quote ,x))
		 ((memq x unlikely-variables)
		  (warning 
		   (sprintf "reference to variable `~s' possibly unintended" x) )))
	   (resolve-variable x e se dest ldest h))
	  ((not-pair? x)
	   (if (constant? x)
	       `(quote ,x)
	       (##sys#syntax-error/context "illegal atomic form" x)))
	  ((symbol? (car x))
	   (let ((ln (or (get-line x) outer-ln)))
	     (emit-syntax-trace-info x #f)
	     (unless (proper-list? x)
	       (if ln
		   (##sys#syntax-error/context (sprintf "(~a) - malformed expression" ln) x)
		   (##sys#syntax-error/context "malformed expression" x)))
	     (set! ##sys#syntax-error-culprit x)
	     (let* ((name0 (lookup (car x) se))
		    (name (or (and (symbol? name0) (##sys#get name0 '##core#primitive)) name0))
		    (xexpanded (##sys#expand x se compiler-syntax-enabled)))
	       (when ln (update-line-number-database! xexpanded ln))
	       (cond ((not (eq? x xexpanded))
		      (walk xexpanded e se dest ldest h ln))
		     
		     [(and inline-table-used (##sys#hash-table-ref inline-table name))
		      => (lambda (val)
			   (walk (cons val (cdr x)) e se dest ldest h ln)) ]
		     
		     [else
		      (case name
			
			((##core#if)
			 `(if
			   ,(walk (cadr x) e se #f #f h ln)
			   ,(walk (caddr x) e se #f #f h ln)
			   ,(if (null? (cdddr x)) 
				'(##core#undefined)
				(walk (cadddr x) e se #f #f h ln) ) ) )

			((##core#syntax ##core#quote)
			 `(quote ,(##sys#strip-syntax (cadr x))))

			((##core#check)
			 (if unsafe
			     ''#t
			     (walk (cadr x) e se dest ldest h ln) ) )

			((##core#the)
			 `(##core#the
			   ,(##sys#strip-syntax (cadr x))
			   ,(caddr x)
			   ,(walk (cadddr x) e se dest ldest h ln)))

			((##core#typecase)
			 `(##core#typecase
			   ,(or ln (cadr x))
			   ,(walk (caddr x) e se #f #f h ln)
			   ,@(map (lambda (cl)
				    (list (##sys#strip-syntax (car cl))
					  (walk (cadr cl) e se dest ldest h ln)))
				  (cdddr x))))

			((##core#immutable)
			 (let ((c (cadadr x)))
			   (cond [(assoc c immutable-constants) => cdr]
				 [else
				  (let ([var (gensym 'c)])
				    (set! immutable-constants (alist-cons c var immutable-constants))
				    (mark-variable var '##compiler#always-bound)
				    (hide-variable var)
				    var) ] ) ) )

			((##core#undefined ##core#callunit ##core#primitive) x)
			
			((##core#inline_ref) 
			 `(##core#inline_ref 
			   (,(caadr x) ,(##sys#strip-syntax (cadadr x)))))

			((##core#inline_loc_ref)
			 `(##core#inline_loc_ref 
			   ,(##sys#strip-syntax (cadr x))
			   ,(walk (caddr x) e se dest ldest h ln)))

			((##core#require-for-syntax)
			 (let ([ids (map eval (cdr x))])
			   (apply ##sys#require ids)
			   (##sys#hash-table-update! 
			    file-requirements 'dynamic/syntax 
			    (cut lset-union eq? <> ids)
			    (lambda () ids) )
			   '(##core#undefined) ) )

			((##core#require-extension)
			 (let ((imp? (caddr x)))
			   (walk
			    (let loop ([ids (##sys#strip-syntax (cadr x))])
			      (if (null? ids)
				  '(##core#undefined)
				  (let ((id (car ids)))
				    (let-values (((exp f realid)
						  (##sys#do-the-right-thing id #t imp?)))
				      (unless (or f 
						  (and (symbol? id)
						       (or (feature? id)
							   (##sys#find-extension
							    (##sys#canonicalize-extension-path 
							     id 'require-extension)
							    #f)) ) ) 
					(warning 
					 (sprintf "extension `~A' is currently not installed" realid)))
				      `(##core#begin ,exp ,(loop (cdr ids))) ) ) ) )
			    e se dest ldest h ln) ) )

			((##core#let)
			 (let* ((bindings (cadr x))
				(vars (unzip1 bindings))
				(aliases (map gensym vars))
				(se2 (##sys#extend-se se vars aliases)))
			   (set-real-names! aliases vars)
			   `(let
			     ,(map (lambda (alias b)
				     (list alias (walk (cadr b) e se (car b) #t h ln)) )
				   aliases bindings)
			     ,(walk (##sys#canonicalize-body 
				     (cddr x) se2 compiler-syntax-enabled)
				    (append aliases e)
				    se2 dest ldest h ln) ) )  )

			((##core#letrec)
			 (let ((bindings (cadr x))
			       (body (cddr x)) )
			   (walk
			    `(##core#let
			      ,(map (lambda (b)
				      (list (car b) '(##core#undefined))) 
				    bindings)
			      ,@(map (lambda (b)
				       `(##core#set! ,(car b) ,(cadr b))) 
				     bindings)
			      (##core#let () ,@body) )
			    e se dest ldest h ln)))

			((##core#lambda)
			 (let ((llist (cadr x))
			       (obody (cddr x)) )
			   (when (##sys#extended-lambda-list? llist)
			     (set!-values 
			      (llist obody) 
			      (##sys#expand-extended-lambda-list 
			       llist obody ##sys#error se) ) )
			   (decompose-lambda-list
			    llist
			    (lambda (vars argc rest)
			      (let* ((aliases (map gensym vars))
				     (se2 (##sys#extend-se se vars aliases))
				     (body0 (##sys#canonicalize-body 
					     obody se2 compiler-syntax-enabled))
				     (body (walk body0 (append aliases e) se2 #f #f dest ln))
				     (llist2 
				      (build-lambda-list
				       aliases argc
				       (and rest (list-ref aliases (posq rest vars))) ) )
				     (l `(##core#lambda ,llist2 ,body)) )
				(set-real-names! aliases vars)
				(cond ((or (not dest) 
					   ldest
					   (assq dest se)) ; not global?
				       l)
				      ((and emit-profile
					    (or (eq? profiled-procedures 'all)
						(and
						 (eq? profiled-procedures 'some)
						 (variable-mark dest '##compiler#profile))))
				       (expand-profile-lambda
					(if (memq dest e) ; should normally not be the case
					    e
					    (##sys#alias-global-hook dest #f #f))
					llist2 body) )
				      (else l)))))))
			
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
			   (walk
			    (##sys#canonicalize-body (cddr x) se2 compiler-syntax-enabled)
			    e se2
			    dest ldest h ln) ) )
			       
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
			  (walk
			   (##sys#canonicalize-body (cddr x) se2 compiler-syntax-enabled)
			   e se2 dest ldest h ln)))
			       
		       ((##core#define-syntax)
			(##sys#check-syntax
			 (car x) x
			 (if (pair? (cadr x))
			     '(_ (variable . lambda-list) . #(_ 1))
			     '(_ variable _) )
			 #f se)
			(let* ((var (if (pair? (cadr x)) (caadr x) (cadr x)))
			       (body (if (pair? (cadr x))
					 `(##core#lambda ,(cdadr x) ,@(cddr x))
					 (caddr x)))
			       (name (lookup var se)))
			  (##sys#register-syntax-export name (##sys#current-module) body)
			  (##sys#extend-macro-environment
			   name
			   (##sys#current-environment)
			   (##sys#eval/meta body))
			  (walk
			   (if ##sys#enable-runtime-macros
			       `(##sys#extend-macro-environment
				 ',var
				 (##sys#current-environment) ,body) ;XXX possibly wrong se?
			       '(##core#undefined) )
			   e se dest ldest h ln)) )

		       ((##core#define-compiler-syntax)
			(let* ((var (cadr x))
			       (body (caddr x))
			       (name (lookup var se)))
			  (when body
			    (set! compiler-syntax
			      (alist-cons
			       name
			       (##sys#get name '##compiler#compiler-syntax)
			       compiler-syntax)))
			  (##sys#put! 
			   name '##compiler#compiler-syntax
			   (and body
				(##sys#cons
				 (##sys#ensure-transformer
				  (##sys#eval/meta body)
				  (##sys#strip-syntax var))
				 (##sys#current-environment))))
			  (walk 
			   (if ##sys#enable-runtime-macros
			       `(##sys#put! 
				(##core#syntax ,name)
				'##compiler#compiler-syntax
				,(and body
				      `(##sys#cons
					(##sys#ensure-transformer 
					 ,body
					 ',var)
					(##sys#current-environment))))
			       '(##core#undefined) )
			   e se dest ldest h ln)))

		       ((##core#let-compiler-syntax)
			(let ((bs (map
				   (lambda (b)
				     (##sys#check-syntax
				      'let-compiler-syntax b '(symbol . #(_ 0 1)))
				     (let ((name (lookup (car b) se)))
				       (list 
					name 
					(and (pair? (cdr b))
					     (cons (##sys#ensure-transformer
						    (##sys#eval/meta (cadr b))
						    (##sys#strip-syntax (car b)))
						   se))
					(##sys#get name '##compiler#compiler-syntax) ) ) )
				   (cadr x))))
			  (dynamic-wind
			      (lambda ()
				(for-each
				 (lambda (b) 
				   (##sys#put! (car b) '##compiler#compiler-syntax (cadr b)))
				 bs) )
			      (lambda ()
				(walk 
				 (##sys#canonicalize-body
				  (cddr x) se compiler-syntax-enabled)
				 e se dest ldest h ln) )
			      (lambda ()
				(for-each
				 (lambda (b)
				   (##sys#put! 
				    (car b)
				    '##compiler#compiler-syntax (caddr b)))
				 bs) ) ) ) )

		       ((##core#include)
			(walk
			 `(##core#begin
			   ,@(fluid-let ((##sys#default-read-info-hook read-info-hook))
			       (##sys#include-forms-from-file (cadr x))))
			 e se dest ldest h ln))

		       ((##core#let-module-alias)
			(##sys#with-module-aliases
			 (map (lambda (b)
				(##sys#check-syntax 'functor b '(symbol symbol))
				(##sys#strip-syntax b))
			      (cadr x))
			 (lambda ()
			   (walk `(##core#begin ,@(cddr x)) e se dest ldest h ln))))

		       ((##core#module)
			(let* ((name (##sys#strip-syntax (cadr x)))
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
					 (##sys#strip-syntax (caddr x)))))
			       (csyntax compiler-syntax))
			  (when (##sys#current-module)
			    (##sys#syntax-error-hook
			     'module "modules may not be nested" name))
			  (let-values (((body mreg)
					(parameterize ((##sys#current-module 
							(##sys#register-module name exports) )
						       (##sys#current-environment '())
						       (##sys#macro-environment
							##sys#initial-macro-environment)
						       (##sys#module-alias-environment
							(##sys#module-alias-environment)))
					  (##sys#with-property-restore
					   (lambda ()
					     (let loop ((body (cdddr x)) (xs '()))
					       (cond 
						((null? body)
						 (handle-exceptions ex
						     (begin
						       ;; avoid backtrace
						       (print-error-message ex (current-error-port))
						       (exit 1))
						   (##sys#finalize-module (##sys#current-module)))
						 (cond ((or all-import-libraries
							    (assq name import-libraries) ) =>
							    (lambda (il)
							      (when enable-module-registration
								(emit-import-lib name il))
							      (values
							       (reverse xs)
							       '((##core#undefined)))))
						       ((not enable-module-registration)
							(values 
							 (reverse xs)
							 '((##core#undefined))))
						       (else
							(values
							 (reverse xs)
							 (if standalone-executable
							     '()
							     (##sys#compiled-module-registration 
							      (##sys#current-module)))))))
						(else
						 (loop 
						  (cdr body)
						  (cons (walk 
							 (car body)
							 e ;?
							 (##sys#current-environment)
							 #f #f h ln)
							xs))))))))))
			    (let ((body
				   (canonicalize-begin-body
				    (append
				     (parameterize ((##sys#current-module #f)
						    (##sys#macro-environment 
						     (##sys#meta-macro-environment)))
				       (map
					(lambda (x)
					  (walk 
					   x 
					   e ;?
					   (##sys#current-meta-environment) #f #f h ln) )
					mreg))
				     body))))
			      (do ((cs compiler-syntax (cdr cs)))
				  ((eq? cs csyntax))
				(##sys#put! (caar cs) '##compiler#compiler-syntax (cdar cs)))
			      (set! compiler-syntax csyntax)
			      body))))

		       ((##core#loop-lambda) ;XXX is this really needed?
			(let* ([vars (cadr x)]
			       [obody (cddr x)]
			       [aliases (map gensym vars)]
			       (se2 (##sys#extend-se se vars aliases))
			       [body 
				(walk 
				 (##sys#canonicalize-body obody se2 compiler-syntax-enabled)
				 (append aliases e) 
				 se2 #f #f dest ln) ] )
			  (set-real-names! aliases vars)
			  `(##core#lambda ,aliases ,body) ) )

			((##core#set!)
			 (let* ([var0 (cadr x)]
				[var (lookup var0 se)]
				[ln (get-line x)]
				[val (caddr x)] )
			   (when (memq var unlikely-variables)
			     (warning 
			      (sprintf "assignment to variable `~s' possibly unintended"
				var)))
			   (cond ((assq var foreign-variables)
				   => (lambda (fv)
					(let ([type (second fv)]
					      [tmp (gensym)] )
					  (walk
					   `(let ([,tmp ,(foreign-type-convert-argument val type)])
					      (##core#inline_update 
					       (,(third fv) ,type)
					       ,(foreign-type-check tmp type) ) )
					   e se #f #f h ln))))
				 ((assq var location-pointer-map)
				  => (lambda (a)
				       (let* ([type (third a)]
					      [tmp (gensym)] )
					 (walk
					  `(let ([,tmp ,(foreign-type-convert-argument val type)])
					     (##core#inline_loc_update 
					      (,type)
					      ,(second a)
					      ,(foreign-type-check tmp type) ) )
					  e se #f #f h ln))))
				 (else
				  (unless (memq var e) ; global?
				    (set! var (or (##sys#get var '##core#primitive)
						  (##sys#alias-global-hook var #t dest)))
				    (when safe-globals-flag
				      (mark-variable var '##compiler#always-bound-to-procedure)
				      (mark-variable var '##compiler#always-bound)))
				  (cond ((##sys#macro? var)
					 (warning 
					  (sprintf "assigned global variable `~S' is syntax ~A"
					    var
					    (if ln (sprintf "(~a)" ln) "") ))
					 (when undefine-shadowed-macros (##sys#undefine-macro! var) ) )
					((and ##sys#notices-enabled
					      (assq var (##sys#current-environment)))
					 (##sys#notice "assignment to imported value binding" var)))
				  (when (keyword? var)
				    (warning (sprintf "assignment to keyword `~S'" var) ))
				  `(set! ,var ,(walk val e se var0 (memq var e) h ln))))))

			((##core#inline)
			 `(##core#inline
			   ,(unquotify (cadr x) se) ,@(mapwalk (cddr x) e se h ln)))

			((##core#inline_allocate)
			 `(##core#inline_allocate 
			   ,(map (cut unquotify <> se) (second x))
			   ,@(mapwalk (cddr x) e se h ln)))

			((##core#inline_update)
			 `(##core#inline_update ,(cadr x) ,(walk (caddr x) e se #f #f h ln)) )

			((##core#inline_loc_update)
			 `(##core#inline_loc_update 
			   ,(cadr x) 
			   ,(walk (caddr x) e se #f #f h ln)
			   ,(walk (cadddr x) e se #f #f h ln)) )

			((##core#compiletimetoo ##core#elaborationtimetoo)
			 (let ((exp (cadr x)))
			   (##sys#eval/meta exp)
			   (walk exp e se dest #f h ln) ) )

			((##core#compiletimeonly ##core#elaborationtimeonly)
			 (##sys#eval/meta (cadr x))
			 '(##core#undefined) )

			((##core#begin ##core#toplevel-begin) 
			 (if (pair? (cdr x))
			     (canonicalize-begin-body
			      (let fold ([xs (cdr x)])
				(let ([x (car xs)]
				      [r (cdr xs)] )
				  (if (null? r)
				      (list (walk x e se dest ldest h ln))
				      (cons (walk x e se #f #f h ln) (fold r)) ) ) ) )
			     '(##core#undefined) ) )

			((##core#foreign-lambda)
			 (walk (expand-foreign-lambda x #f) e se dest ldest h ln) )

			((##core#foreign-safe-lambda)
			 (walk (expand-foreign-lambda x #t) e se dest ldest h ln) )

			((##core#foreign-lambda*)
			 (walk (expand-foreign-lambda* x #f) e se dest ldest h ln) )

			((##core#foreign-safe-lambda*)
			 (walk (expand-foreign-lambda* x #t) e se dest ldest h ln) )

			((##core#foreign-primitive)
			 (walk (expand-foreign-primitive x) e se dest ldest h ln) )

			((##core#define-foreign-variable)
			 (let* ([var (##sys#strip-syntax (second x))]
				[type (##sys#strip-syntax (third x))]
				[name (if (pair? (cdddr x))
					  (fourth x)
					  (symbol->string var) ) ] )
			   (set! foreign-variables
			     (cons (list var type
					 (if (string? name)
					     name 
					     (symbol->string name)))
				   foreign-variables))
			   '(##core#undefined) ) )

			((##core#define-foreign-type)
			 (let ([name (second x)]
			       [type (##sys#strip-syntax (third x))] 
			       [conv (cdddr x)] )
			   (cond [(pair? conv)
				  (let ([arg (gensym)]
					[ret (gensym)] )
				    (##sys#hash-table-set! foreign-type-table name (vector type arg ret))
				    (mark-variable arg '##compiler#always-bound)
				    (mark-variable ret '##compiler#always-bound)
				    (hide-variable arg)
				    (hide-variable ret)
				    (walk
				     `(##core#begin
					(define ,arg ,(first conv))
					(define 
					 ,ret 
					 ,(if (pair? (cdr conv)) (second conv) '##sys#values)) ) 
				     e se dest ldest h ln) ) ]
				 [else
				  (##sys#hash-table-set! foreign-type-table name type)
				  '(##core#undefined) ] ) ) )

			((##core#define-external-variable)
			 (let* ([sym (second x)]
				[name (symbol->string sym)]
				[type (third x)] 
				[exported (fourth x)]
				[rname (make-random-name)] )
			   (unless exported (set! name (symbol->string (fifth x))))
			   (set! external-variables (cons (vector name type exported) external-variables))
			   (set! foreign-variables
			     (cons (list rname 'c-pointer (string-append "&" name))
				   foreign-variables) )
			   (set! external-to-pointer (alist-cons sym rname external-to-pointer))
			   '(##core#undefined) ) )

			((##core#let-location)
			 (let* ([var (second x)]
				[type (##sys#strip-syntax (third x))]
				[alias (gensym)]
				[store (gensym)] 
				[init (and (pair? (cddddr x)) (fourth x))] )
			   (set-real-name! alias var)
			   (set! location-pointer-map
			     (cons (list alias store type) location-pointer-map) )
			   (walk
			    `(let (,(let ([size (words (estimate-foreign-result-location-size type))])
				      ;; Add 2 words: 1 for the header, 1 for double-alignment:
				      ;; Note: C_a_i_bytevector takes number of words, not bytes
				      (list 
				       store
				       `(##core#inline_allocate
					 ("C_a_i_bytevector" ,(+ 2 size))
					 ',size)) ) )
			       (##core#begin
				,@(if init
				      `((##core#set! ,alias ,init))
				      '() )
				,(if init (fifth x) (fourth x)) ) )
			    e (alist-cons var alias se)
			    dest ldest h ln) ) )

			((##core#define-inline)
			 (let* ((name (second x))
				(val `(##core#lambda ,@(cdaddr x))))
			     (##sys#hash-table-set! inline-table name val)
			     (set! inline-table-used #t)
			     '(##core#undefined)))

			((##core#define-constant)
			 (let* ([name (second x)]
				[valexp (third x)]
				[val (handle-exceptions ex
					 ;; could show line number here
					 (quit "error in constant evaluation of ~S for named constant `~S'" 
					       valexp name)
				       (if (and (not (symbol? valexp))
						(collapsable-literal? valexp))
					   valexp
					   (eval
					    `(##core#let
					      ,defconstant-bindings ,valexp)) ) ) ] )
			   (set! constants-used #t)
			   (set! defconstant-bindings
			     (cons (list name `',val)  defconstant-bindings))
			   (cond ((collapsable-literal? val)
				  (##sys#hash-table-set! constant-table name (list val))
				  '(##core#undefined) )
				 ((basic-literal? val)
				  (let ([var (gensym "constant")])
				    (##sys#hash-table-set! constant-table name (list var))
				    (hide-variable var)
				    (mark-variable var '##compiler#constant)
				    (mark-variable var '##compiler#always-bound)
				    (walk `(define ,var ',val) e se #f #f h ln) ) )
				 (else
				  (quit "invalid compile-time value for named constant `~S'"
					name)))))

			((##core#declare)
			 (walk
			  `(##core#begin
			     ,@(map (lambda (d)
				      (process-declaration 
				       d se
				       (lambda (id)
					 (memq (lookup id se) e))))
				    (cdr x) ) )
			  e '() #f #f h ln) )
	     
			((##core#foreign-callback-wrapper)
			 (let-values ([(args lam) (split-at (cdr x) 4)])
			   (let* ([lam (car lam)]
				  [raw-c-name (cadr (first args))]
                                  [name (##sys#alias-global-hook raw-c-name #t dest)]
				  [rtype (cadr (third args))]
				  [atypes (cadr (fourth args))]
				  [vars (second lam)] )
			     (if (valid-c-identifier? raw-c-name)
				 (set! callback-names
				   (cons (cons raw-c-name name) callback-names))
				 (quit "name `~S' of external definition is not a valid C identifier"
				       raw-c-name) )
			     (when (or (not (proper-list? vars)) 
				       (not (proper-list? atypes))
				       (not (= (length vars) (length atypes))) )
			       (syntax-error 
				"non-matching or invalid argument list to foreign callback-wrapper"
				vars atypes) )
			     `(##core#foreign-callback-wrapper
			       ,@(mapwalk args e se h ln)
			       ,(walk `(##core#lambda 
					,vars
					(##core#let
					 ,(let loop ([vars vars] [types atypes])
					    (if (null? vars)
						'()
						(let ([var (car vars)]
						      [type (car types)] )
						  (cons 
						   (list 
						    var
						    (foreign-type-convert-result
						     (finish-foreign-result
						      (final-foreign-type type) 
						      var)
						     type) )
						   (loop (cdr vars) (cdr types)) ) ) ) )
					 ,(foreign-type-convert-argument
					   `(##core#let
					     ()
					     ,@(cond
						((member 
						  rtype
						  '((const nonnull-c-string) 
						    (const nonnull-unsigned-c-string)
						    nonnull-unsigned-c-string
						    nonnull-c-string))
						 `((##sys#make-c-string
						    (##core#let
						     () ,@(cddr lam))
                                                    ',name)))
						((member 
						  rtype
						  '((const c-string*)
						    (const unsigned-c-string*)
						    unsigned-c-string*
						    c-string*
						    c-string-list
						    c-string-list*))
						 (syntax-error
						  "not a valid result type for callback procedures"
						  rtype
						  name) )
						((member 
						  rtype
						  '(c-string
						    (const unsigned-c-string)
						    unsigned-c-string
						    (const c-string)) )
						 `((##core#let
						    ((r (##core#let () ,@(cddr lam))))
						    (,(macro-alias 'and se)
						     r 
						     (##sys#make-c-string r ',name)) ) ) )
						(else (cddr lam)) ) )
					   rtype) ) )
				      e se #f #f h ln) ) ) ) )

			((##core#location)
			 (let ([sym (cadr x)])
			   (if (symbol? sym)
			       (cond [(assq (lookup sym se) location-pointer-map)
				      => (lambda (a)
					   (walk
					    `(##sys#make-locative ,(second a) 0 #f 'location)
					    e se #f #f h ln) ) ]
				     [(assq sym external-to-pointer) 
				      => (lambda (a) (walk (cdr a) e se #f #f h ln)) ]
				     [(assq sym callback-names)
				      `(##core#inline_ref (,(symbol->string sym) c-pointer)) ]
				     [else 
				      (walk 
				       `(##sys#make-locative ,sym 0 #f 'location) 
				       e se #f #f h ln) ] )
			       (walk 
				`(##sys#make-locative ,sym 0 #f 'location) 
				e se #f #f h ln) ) ) )
				 
			(else
			 (let* ((x2 (fluid-let ((##sys#syntax-context
						 (cons name ##sys#syntax-context)))
				      (mapwalk x e se h ln)))
				(head2 (car x2))
				(old (##sys#hash-table-ref line-number-database-2 head2)) )
			   (when ln
			     (##sys#hash-table-set!
			      line-number-database-2
			      head2
			      (cons name (alist-cons x2 ln (if old (cdr old) '()))) ) )
			   x2) ) ) ] ) ) ) )

	  ((not (proper-list? x))
	   (##sys#syntax-error/context "malformed expression" x) )

	  ((constant? (car x))
	   (emit-syntax-trace-info x #f)
	   (warning "literal in operator position" x) 
	   (mapwalk x e se h outer-ln) )

	  (else
	   (emit-syntax-trace-info x #f)
	   (let ((tmp (gensym)))
	     (walk
	      `(##core#let 
		((,tmp ,(car x)))
		(,tmp ,@(cdr x)))
	      e se dest ldest h outer-ln)))))
  
  (define (mapwalk xs e se h ln)
    (map (lambda (x) (walk x e se #f #f h ln)) xs) )

  (when (memq 'c debugging-chicken) (newline) (pretty-print exp))
  (##sys#clear-trace-buffer)
  ;; Process visited definitions and main expression:
  (walk 
   `(##core#begin
     ,@(let ([p (reverse pending-canonicalizations)])
	 (set! pending-canonicalizations '())
	 p)
     ,(begin
	(set! extended-bindings (append internal-bindings extended-bindings))
	exp) )
   '() (##sys#current-environment) #f #f #f #f) ) )


(define (process-declaration spec se local?)
  (define (check-decl spec minlen . maxlen)
    (let ([n (length (cdr spec))])
      (if (or (< n minlen) (> n (optional maxlen 99999)))
	  (syntax-error "invalid declaration" spec) ) ) )  
  (define (stripa x)			; global aliasing
    (##sys#globalize x se))
  (define (strip x)			; raw symbol
    (##sys#strip-syntax x))
  (define stripu ##sys#strip-syntax)
  (define (globalize-all syms)
    (filter-map
     (lambda (var)
       (cond ((local? var) 
	      (note-local var)
	      #f)
	     (else (##sys#globalize var se))))
     syms))
  (define (note-local var)
    (##sys#notice 
     (sprintf "ignoring declaration for locally bound variable `~a'" var)))
  (call-with-current-continuation
   (lambda (return)
     (unless (pair? spec)
       (syntax-error "invalid declaration specification" spec) )
     ;(pp `(DECLARE: ,(strip spec)))
     (case (##sys#strip-syntax (car spec)) ; no global aliasing
       ((uses)
	(let ((us (stripu (cdr spec))))
	  (apply register-feature! us)
	  (when (pair? us)
	    (##sys#hash-table-update! 
	     file-requirements 'static
	     (cut lset-union eq? us <>) 
	     (lambda () us))
	    (let ((units (map (lambda (u) (string->c-identifier (stringify u))) us)))
	      (set! used-units (append used-units units)) ) ) ) )
       ((unit)
	(check-decl spec 1 1)
	(let* ([u (stripu (cadr spec))]
	       [un (string->c-identifier (stringify u))] )
	  (when (and unit-name (not (string=? unit-name un)))
	    (warning "unit was already given a name (new name is ignored)") )
	  (set! unit-name un) ) )
       ((standard-bindings)
	(if (null? (cdr spec))
	    (set! standard-bindings default-standard-bindings)
	    (set! standard-bindings (append (stripa (cdr spec)) standard-bindings)) ) )
       ((extended-bindings)
	(if (null? (cdr spec))
	    (set! extended-bindings default-extended-bindings)
	    (set! extended-bindings (append (stripa (cdr spec)) extended-bindings)) ) )
       ((usual-integrations)      
	(cond [(null? (cdr spec))
	       (set! standard-bindings default-standard-bindings)
	       (set! extended-bindings default-extended-bindings) ]
	      [else
	       (let ([syms (stripa (cdr spec))])
		 (set! standard-bindings (lset-intersection eq? syms default-standard-bindings))
		 (set! extended-bindings (lset-intersection eq? syms default-extended-bindings)) ) ] ) )
       ((number-type)
	(check-decl spec 1 1)
	(set! number-type (strip (cadr spec))))
       ((fixnum fixnum-arithmetic) (set! number-type 'fixnum))
       ((generic) (set! number-type 'generic))
       ((unsafe) (set! unsafe #t))
       ((safe) (set! unsafe #f))
       ((no-bound-checks) (set! no-bound-checks #t))
       ((no-argc-checks) (set! no-argc-checks #t))
       ((no-procedure-checks) (set! no-procedure-checks #t))
       ((interrupts-enabled) (set! insert-timer-checks #t))
       ((disable-interrupts) (set! insert-timer-checks #f))
       ((always-bound) 
	(for-each (cut mark-variable <> '##compiler#always-bound) (stripa (cdr spec))))
       ((safe-globals) (set! safe-globals-flag #t))
       ((no-procedure-checks-for-usual-bindings)
	(for-each 
	 (cut mark-variable <> '##compiler#always-bound-to-procedure)
	 (append default-standard-bindings default-extended-bindings))
	(for-each
	 (cut mark-variable <> '##compiler#always-bound)
	 (append default-standard-bindings default-extended-bindings)))
       ((no-procedure-checks-for-toplevel-bindings)
	(set! no-global-procedure-checks #t))
       ((bound-to-procedure)
	(let ((vars (globalize-all (cdr spec))))
	  (for-each (cut mark-variable <> '##compiler#always-bound-to-procedure) vars)
	  (for-each (cut mark-variable <> '##compiler#always-bound) vars)))
       ((foreign-declare)
	(let ([fds (cdr spec)])
	  (if (every string? fds)
	      (set! foreign-declarations (append foreign-declarations fds))
	      (syntax-error 'declare "invalid declaration" spec) ) ) )
       ((block) (set! block-compilation #t))
       ((separate) (set! block-compilation #f))
       ((keep-shadowed-macros) (set! undefine-shadowed-macros #f))
       ((unused)
	(for-each (cut mark-variable <> '##compiler#unused) (globalize-all (cdr spec))))
       ((enforce-argument-types)
	(for-each
	 (cut mark-variable <> '##compiler#enforce)
	 (globalize-all (cdr spec))))
       ((not)
	(check-decl spec 1)
	(case (##sys#strip-syntax (second spec)) ; strip all
	  [(standard-bindings)
	   (if (null? (cddr spec))
	       (set! standard-bindings '())
	       (set! standard-bindings
		 (lset-difference eq? default-standard-bindings
				  (stripa (cddr spec))))) ]
	  [(extended-bindings)
	   (if (null? (cddr spec))
	       (set! extended-bindings '())
	       (set! extended-bindings 
		 (lset-difference eq? default-extended-bindings
				  (stripa (cddr spec))) )) ]
	  [(inline)
	   (if (null? (cddr spec))
	       (set! inline-locally #f)
	       (for-each 
		(cut mark-variable <> '##compiler#inline 'no)
		(globalize-all (cddr spec)))) ]
	  [(usual-integrations)      
	   (cond [(null? (cddr spec))
		  (set! standard-bindings '())
		  (set! extended-bindings '()) ]
		 [else
		  (let ([syms (stripa (cddr spec))])
		    (set! standard-bindings (lset-difference eq? default-standard-bindings syms))
		    (set! extended-bindings (lset-difference eq? default-extended-bindings syms)) ) ] ) ]
	  ((inline-global)
	   (set! enable-inline-files #t)
	   (when (pair? (cddr spec))
	     (for-each
	      (cut mark-variable <> '##compiler#inline-global 'no)
	      (globalize-all (cddr spec)))))
	  [else
	   (check-decl spec 1 1)
	   (let ((id (strip (cadr spec))))
	     (case id
	       [(interrupts-enabled) (set! insert-timer-checks #f)]
	       [(safe) (set! unsafe #t)]
	       [else (warning "unsupported declaration specifier" id)]))]))
       ((compile-syntax)
	(set! ##sys#enable-runtime-macros #t))
       ((block-global hide) 
	(let ([syms (globalize-all (cdr spec))])
	  (if (null? syms)
	      (set! block-compilation #t)
	      (for-each hide-variable syms))))
       ((export)
	(set! block-compilation #t)
	(let ((syms (globalize-all (cdr spec))))
	  (for-each export-variable syms)))
       ((emit-external-prototypes-first)
	(set! external-protos-first #t) )
       ((inline)
	(if (null? (cdr spec))
	    (set! inline-locally #t)
	    (for-each
	     (cut mark-variable <> '##compiler#inline 'yes)
	     (globalize-all (cdr spec)))))
       ((inline-limit)
	(check-decl spec 1 1)
	(let ([n (cadr spec)])
	  (if (number? n)
	      (set! inline-max-size n)
	      (warning 
	       "invalid argument to `inline-limit' declaration"
	       spec) ) ) )
       ((constant			; DEPRECATED
	 pure)
	(let ((syms (cdr spec)))
	  (if (every symbol? syms)
	      (for-each 
	       (cut mark-variable <> '##compiler#pure #t) 
	       (globalize-all syms))
	      (quit "invalid arguments to `constant' declaration: ~S" spec)) ) )
       ((emit-import-library)
	(set! import-libraries
	  (append
	   import-libraries
	   (map (lambda (il)
		  (cond ((symbol? il)
			 (cons il (string-append (symbol->string il) ".import.scm")) )
			((and (list? il) (= 2 (length il))
			      (symbol? (car il)) (string (cadr il)))
			 (cons (car il) (cadr il))) 
			(else
			 (warning 
			  "invalid import-library specification" il))))
		(strip (cdr spec))))))
       ((profile)
	(set! emit-profile #t)
	(cond ((null? (cdr spec))
	       (set! profiled-procedures 'all) )
	      (else
	       (set! profiled-procedures 'some)
	       (for-each 
		(cut mark-variable <> '##compiler#profile)
		(globalize-all (cdr spec))))))
       ((local)
	(cond ((null? (cdr spec))
	       (set! local-definitions #t) )
	      (else
	       (for-each 
		(cut mark-variable <> '##compiler#local)
		(stripa (cdr spec))))))
       ((inline-global)
	(set! enable-inline-files #t)
	(set! inline-locally #t)
	(when (pair? (cdr spec))
	  (for-each
	   (cut mark-variable <> '##compiler#inline-global 'yes)
	   (globalize-all (cdr spec)))))
       ((type)
	(for-each
	 (lambda (spec)
	   (if (not (and (list? spec)
			 (>= (length spec) 2)
			 (symbol? (car spec))))
	       (warning "illegal type declaration" (##sys#strip-syntax spec))
	       (let ((name (##sys#globalize (car spec) se))
		     (type (##sys#strip-syntax (cadr spec))))
		 (if (local? (car spec))
		     (note-local (car spec))
		     (let-values (((type pred pure) (validate-type type name)))
		       (cond (type
			      ;; HACK: since `:' doesn't have access to the SE, we
			      ;; fixup the procedure name if type is a named procedure type
			      ;; (We only have access to the SE for ##sys#globalize in here).
			      ;; Quite terrible.
			      (when (and (pair? type) 
					 (eq? 'procedure (car type)) 
					 (symbol? (cadr type)))
				(set-car! (cdr type) name))
			      (mark-variable name '##compiler#type type)
			      (mark-variable name '##compiler#declared-type)
			      (when pure
				(mark-variable name '##compiler#pure #t))
			      (when pred
				(mark-variable name '##compiler#predicate pred))
			      (when (pair? (cddr spec))
				(install-specializations 
				 name 
				 (##sys#strip-syntax (cddr spec)))))
			     (else
			      (warning 
			       "illegal `type' declaration"
			       (##sys#strip-syntax spec)))))))))
	 (cdr spec)))
       ((predicate)
	(for-each
	 (lambda (spec)
	   (cond ((and (list? spec) (symbol? (car spec)) (= 2 (length spec)))
		  (let ((name (##sys#globalize (car spec) se))
			(type (##sys#strip-syntax (cadr spec))))
		    (if (local? (car spec))
			(note-local (car spec))
			(let-values (((type pred pure) (validate-type type name)))
			  (if (and type (not pred))
			      (mark-variable name '##compiler#predicate type)
			      (warning "illegal `predicate' declaration" spec))))))
		 (else
		  (warning "illegal `type' declaration item" spec))))
	 (cdr spec)))
       ((specialize)
	(set! enable-specialization #t))
       ((strict-types)
	(set! strict-variable-types #t))
       (else (warning "unknown declaration specifier" spec)) )
     '(##core#undefined) ) ) )


;;; Expand "foreign-lambda"/"foreign-safe-lambda" forms and add item to stub-list:

(define-record-type foreign-stub
  (make-foreign-stub id return-type name argument-types argument-names body cps callback)
  foreign-stub?
  (id foreign-stub-id)			; symbol
  (return-type foreign-stub-return-type)	  ; type-specifier
  (name foreign-stub-name)			  ; string or #f
  (argument-types foreign-stub-argument-types) ; (type-specifier...)
  (argument-names foreign-stub-argument-names) ; #f or (symbol ...)
  (body foreign-stub-body)		       ; #f or string
  (cps foreign-stub-cps)		       ; boolean
  (callback foreign-stub-callback))	       ; boolean

(define (create-foreign-stub rtype sname argtypes argnames body callback cps)
  (let* ((rtype (##sys#strip-syntax rtype))
	 (argtypes (##sys#strip-syntax argtypes))
	 [params (list-tabulate (length argtypes) (lambda (x) (gensym 'a)))]
	 [f-id (gensym 'stub)]
	 [bufvar (gensym)] 
	 [rsize (estimate-foreign-result-size rtype)] )
    (when sname (set-real-name! f-id (string->symbol sname)))
    (set! foreign-lambda-stubs 
      (cons (make-foreign-stub f-id rtype sname argtypes argnames body cps callback)
	    foreign-lambda-stubs) )
    (let ([rsize (if callback (+ rsize 24) rsize)] ; 24 -> has to hold cons on 64-bit platforms!
	  [head (if cps
		    `((##core#primitive ,f-id))
		    `(##core#inline ,f-id) ) ]
	  [rest (map (lambda (p t) (foreign-type-check (foreign-type-convert-argument p t) t)) params argtypes)] )
      `(lambda ,params
	 ;; Do minor GC (if callback) to make room on stack:
	 ,@(if callback '((##sys#gc #f)) '())
	 ,(if (zero? rsize) 
	      (foreign-type-convert-result (append head (cons '(##core#undefined) rest)) rtype)
	      (let ([ft (final-foreign-type rtype)]
		    [ws (words rsize)] )
		`(let ([,bufvar (##core#inline_allocate ("C_a_i_bytevector" ,(+ 2 ws)) ',ws)])
		   ,(foreign-type-convert-result
		     (finish-foreign-result ft (append head (cons bufvar rest)))
		     rtype) ) ) ) ) ) ) )

(define (expand-foreign-lambda exp callback?)
  (let* ([name (third exp)]
	 [sname (cond ((symbol? name) (symbol->string (##sys#strip-syntax name)))
		      ((string? name) name)
		      (else (quit "name `~s' of foreign procedure has wrong type" name)) ) ]
	 [rtype (second exp)]
	 [argtypes (cdddr exp)] )
    (create-foreign-stub rtype sname argtypes #f #f callback? callback?) ) )

(define (expand-foreign-lambda* exp callback?)
  (let* ([rtype (second exp)]
	 [args (third exp)]
	 [body (apply string-append (cdddr exp))]
 	 [argtypes (map (lambda (x) (car x)) args)]
         ;; C identifiers aren't hygienically renamed inside body strings
	 [argnames (map cadr (##sys#strip-syntax args))] )
    (create-foreign-stub rtype #f argtypes argnames body callback? callback?) ) )

;; TODO: Try to fold this procedure into expand-foreign-lambda*
(define (expand-foreign-primitive exp)
  (let* ([hasrtype (and (pair? (cddr exp)) (not (string? (caddr exp))))]
	 [rtype (if hasrtype (second exp) 'void)]
	 [args (##sys#strip-syntax (if hasrtype (third exp) (second exp)))]
	 [body (apply string-append (if hasrtype (cdddr exp) (cddr exp)))]
 	 [argtypes (map (lambda (x) (car x)) args)]
         ;; C identifiers aren't hygienically renamed inside body strings
	 [argnames (map cadr (##sys#strip-syntax args))] )
    (create-foreign-stub rtype #f argtypes argnames body #f #t) ) )


;;; Traverse expression and update line-number db with all contained calls:

(define (update-line-number-database! exp ln)
  (define (mapupdate xs)
    (let loop ((xs xs))
      (when (pair? xs)
	(walk (car xs))
	(loop (cdr xs)) ) ) )
  (define (walk x)
    (cond ((not-pair? x))
	  ((symbol? (car x))
	   (let* ((name (car x))
		  (old (or (##sys#hash-table-ref ##sys#line-number-database name) '())) )
	     (unless (assq x old)
	       (##sys#hash-table-set! ##sys#line-number-database name (alist-cons x ln old)) )
	     (mapupdate (cdr x)) ) )
	  (else (mapupdate x)) ) )
  (walk exp) )


;;; Convert canonicalized node-graph into continuation-passing-style:

(define (perform-cps-conversion node)

  (define (cps-lambda id returnvar llist subs k)
    (let ([t1 (or returnvar (gensym 'k))])
      (k (make-node
	  '##core#lambda (list id #t (cons t1 llist) 0)
	  (list (walk (gensym-f-id)
                      (car subs)
		      (lambda (r) 
			(make-node '##core#call (list #t) (list (varnode t1) r)) ) ) ) ) ) ) )

  (define (node-for-var? node var)
     (and (eq? (node-class node) '##core#variable)
          (eq? (car (node-parameters node)) var)))
  
  (define (walk returnvar n k)
    (let ((subs (node-subexpressions n))
	  (params (node-parameters n)) 
	  (class (node-class n)) )
      (case (node-class n)
	((##core#variable quote ##core#undefined ##core#primitive) (k n))
	((if) (let* ((t1 (gensym 'k))
		     (t2 (or returnvar (gensym 'r)))
		     (k1 (lambda (r) (make-node '##core#call (list #t) (list (varnode t1) r)))) )
		(make-node 
		 'let
		 (list t1)
		 (list (make-node '##core#lambda (list (gensym-f-id) #f (list t2) 0) 
				  (list (k (varnode t2))) )
		       (walk #f (car subs)
			     (lambda (v)
			       (make-node 'if '()
					  (list v
						(walk #f (cadr subs) k1)
						(walk #f (caddr subs) k1) ) ) ) ) ) ) ) )
	((let)
	 (let loop ((vars params) (vals subs))
	   (if (null? vars)
	       (walk #f (car vals) k)
	       (walk (car vars)
                     (car vals)
		     (lambda (r)
                       (if (node-for-var? r (car vars)) ; Don't generate unneccessary lets
                           (loop (cdr vars) (cdr vals))
                           (make-node 'let
                                      (list (car vars))
                                      (list r (loop (cdr vars) (cdr vals))) )) ) ) ) ) )
	((lambda ##core#lambda) (cps-lambda (gensym-f-id) returnvar (first params) subs k))
	((set!) (let ((t1 (gensym 't)))
		  (walk #f
                        (car subs)
			(lambda (r)
			  (make-node 'let (list t1)
				     (list (make-node 'set! (list (first params)) (list r))
					   (k (varnode t1)) ) ) ) ) ) )
	((##core#foreign-callback-wrapper)
	 (let ([id (gensym-f-id)]
	       [lam (first subs)] )
	   (set! foreign-callback-stubs
	     (cons (apply make-foreign-callback-stub id params) foreign-callback-stubs) )
	   ;; mark to avoid leaf-routine optimization
	   (mark-variable id '##compiler#callback-lambda)
           ;; maybe pass returnvar here?
	   (cps-lambda id #f (first (node-parameters lam)) (node-subexpressions lam) k) ) )
	((##core#inline ##core#inline_allocate ##core#inline_ref ##core#inline_update ##core#inline_loc_ref 
			##core#inline_loc_update)
	 (walk-inline-call class params subs k) )
	((##core#call) (walk-call returnvar (car subs) (cdr subs) params k))
	((##core#callunit) (walk-call-unit returnvar (first params) k))
	((##core#the ##core#the/result)
	 ;; remove "the" nodes, as they are not used after scrutiny
	 (walk returnvar (car subs) k))
	((##core#typecase)
	 ;; same here, the last clause is chosen, exp is dropped
	 (walk returnvar (last subs) k))
	(else (bomb "bad node (cps)")) ) ) )
  
  (define (walk-call returnvar fn args params k)
    (let ((t0 (gensym 'k))
          (t3 (or returnvar (gensym 'r))) )
      (make-node
       'let (list t0)
       (list (make-node '##core#lambda (list (gensym-f-id) #f (list t3) 0) 
			(list (k (varnode t3))) )
	     (walk-arguments
	      args
	      (lambda (vars)
		(walk #f fn
		      (lambda (r) 
			(make-node '##core#call params (cons* r (varnode t0) vars) ) ) ) ) ) ) ) ) )
  
  (define (walk-call-unit returnvar unitname k)
    (let ((t0 (gensym 'k))
	  (t3 (or returnvar (gensym 'r))) )
      (make-node
       'let (list t0)
       (list (make-node '##core#lambda (list (gensym-f-id) #f (list t3) 0) 
			(list (k (varnode t3))) )
	     (make-node '##core#callunit (list unitname)
			(list (varnode t0)) ) ) ) ) )

  (define (walk-inline-call class op args k)
    (walk-arguments
     args
     (lambda (vars)
       (k (make-node class op vars)) ) ) )
  
  (define (walk-arguments args wk)
    (let loop ((args args) (vars '()))
      (cond ((null? args) (wk (reverse vars)))
            ((atomic? (car args))
             (loop (cdr args) (cons (car args) vars)) )
            (else
             (let ((t1 (gensym 'a)))
               (walk t1
                     (car args)
                     (lambda (r)
                       (if (node-for-var? r t1) ; Don't generate unneccessary lets
                           (loop (cdr args) (cons (varnode t1) vars) )
                           (make-node 'let (list t1)
                                      (list r
                                            (loop (cdr args) 
                                                  (cons (varnode t1) vars) ) ) )) ) ) ) ) ) ) )
  
  (define (atomic? n)
    (let ((class (node-class n)))
      (or (memq class '(quote ##core#variable ##core#undefined))
	  (and (memq class '(##core#inline_allocate
			     ##core#inline_ref ##core#inline_update
			     ##core#inline_loc_ref ##core#inline_loc_update))
	       (every atomic? (node-subexpressions n)) ) ) ) )
  
  (walk #f node values) )


;;; Foreign callback stub type:

(define-record-type foreign-callback-stub
  (make-foreign-callback-stub id name qualifiers return-type argument-types)
  foreign-callback-stub?
  (id foreign-callback-stub-id)		; symbol
  (name foreign-callback-stub-name)	; string
  (qualifiers foreign-callback-stub-qualifiers)	; string
  (return-type foreign-callback-stub-return-type) ; type-specifier
  (argument-types foreign-callback-stub-argument-types)) ; (type-specifier ...)


;;; Perform source-code analysis:

(define (analyze-expression node)
  ;; Avoid crowded hash tables by using previous run's size as heuristic
  (let* ((db-size (fx* (fxmax current-analysis-database-size 1) 3))
         (db (make-vector db-size '()))
	 (explicitly-consed '()) )

    (define (grow n)
      (set! current-program-size (+ current-program-size n)) )

    ;; fullenv is constantly (append localenv env). It's there to avoid
    ;; exponential behaviour by APPEND calls when compiling deeply nested LETs
    (define (walk n env localenv fullenv here call)
      (let ((subs (node-subexpressions n))
	    (params (node-parameters n)) 
	    (class (node-class n)) )
	(grow 1)
	(case class
	  ((quote ##core#undefined ##core#proc) #f)

	  ((##core#variable)
	   (let ((var (first params)))
	     (ref var n)
	     (unless (memq var localenv)
	       (grow 1)
	       (cond ((memq var env) 
		      (put! db var 'captured #t))
		     ((not (get db var 'global)) 
		      (put! db var 'global #t) ) ) ) ) )
	  
	  ((##core#callunit ##core#recurse)
	   (grow 1)
	   (walkeach subs env localenv fullenv here #f) )

	  ((##core#call)
	   (grow 1)
	   (let ([fun (car subs)])
	     (when (eq? '##core#variable (node-class fun))
	       (let ((name (first (node-parameters fun))))
		 (collect! db name 'call-sites (cons here n))))
	     (walk (first subs) env localenv fullenv here #t)
	     (walkeach (cdr subs) env localenv fullenv here #f) ) )

	  ((let ##core#let)
	   (let ([env2 (append params fullenv)])
	     (let loop ([vars params] [vals subs])
	       (if (null? vars)
		   (walk (car vals) env (append params localenv) env2 here #f)
		   (let ([var (car vars)]
			 [val (car vals)] )
		     (put! db var 'home here)
		     (assign var val env2 here)
		     (walk val env localenv fullenv here #f) 
		     (loop (cdr vars) (cdr vals)) ) ) ) ) )

	  ((lambda) ; this is an intermediate lambda, slightly different
	   (grow 1) ; from '##core#lambda nodes (params = (LLIST));
	   (decompose-lambda-list	; CPS will convert this into ##core#lambda
	    (first params)
	    (lambda (vars argc rest)
	      (for-each 
	       (lambda (var) (put! db var 'unknown #t))
	       vars)
	      (let ([tl toplevel-scope])
		(set! toplevel-scope #f)
		(walk (car subs) fullenv vars (append vars fullenv) #f #f)
		(set! toplevel-scope tl) ) ) ) )

	  ((##core#lambda ##core#direct_lambda)
	   (grow 1)
	   (decompose-lambda-list
	    (third params)
	    (lambda (vars argc rest)
	      (let ([id (first params)]
		    [size0 current-program-size] )
		(when here
		  (collect! db here 'contains id)
		  (put! db id 'contained-in here) )
		(for-each 
		 (lambda (var)
		   (put! db var 'home here)
		   (put! db var 'unknown #t) )
		 vars)
		(when rest
		  (put! db rest 'rest-parameter 'list) )
		(when (simple-lambda-node? n) (put! db id 'simple #t))
		(let ([tl toplevel-scope])
		  (unless toplevel-lambda-id (set! toplevel-lambda-id id))
		  (when (and (second params) (not (eq? toplevel-lambda-id id)))
		    (set! toplevel-scope #f)) ; only if non-CPS lambda
		  (walk (car subs) fullenv vars (append vars fullenv) id #f)
		  (set! toplevel-scope tl)
		  ;; decorate ##core#call node with size
		  (set-car! (cdddr (node-parameters n)) (- current-program-size size0)) ) ) ) ) )
	  
	  ((set! ##core#set!) 
	   (let* ((var (first params))
		  (val (car subs)) )
	     (when (and first-analysis (not bootstrap-mode))
	       (case (variable-mark var '##compiler#intrinsic)
		 ((standard)
		  (warning "redefinition of standard binding" var) )
		 ((extended)
		  (warning "redefinition of extended binding" var) ) ))
	     (put! db var 'potential-value val)
	     (unless (memq var localenv)
	       (grow 1)
	       (cond ((memq var env) 
		      (put! db var 'captured #t))
		     ((not (get db var 'global)) 
		      (put! db var 'global #t) ) ) )
	     (assign var val fullenv here)
	     (unless toplevel-scope (put! db var 'assigned-locally #t))
	     (put! db var 'assigned #t)
	     (walk (car subs) env localenv fullenv here #f) ) )

	  ((##core#primitive ##core#inline)
	   (let ((id (first params)))
	     (when (and first-analysis here (symbol? id) (##sys#hash-table-ref real-name-table id))
	       (set-real-name! id here) )
	     (walkeach subs env localenv fullenv here #f) ) )

	  (else (walkeach subs env localenv fullenv here #f)) ) ) )

    (define (walkeach xs env lenv fenv here call) 
      (for-each (lambda (x) (walk x env lenv fenv here call)) xs) )

    (define (assign var val env here)
      (cond ((eq? '##core#undefined (node-class val))
	     (put! db var 'undefined #t) )
	    ((and (eq? '##core#variable (node-class val)) ; assignment to itself
		  (eq? var (first (node-parameters val))) ) )
	    ((or (memq var env)
		 (variable-mark var '##compiler#constant)
		 (not (variable-visible? var)))
	     (let ((props (get-all db var 'unknown 'value))
		   (home (get db var 'home)) )
	       (unless (assq 'unknown props)
		 (if (assq 'value props)
		     (put! db var 'unknown #t)
		     (if (or (not home) (eq? here home))
			 (put! db var 'value val)
			 (put! db var 'unknown #t) ) ) ) ) )
	    ((and (or local-definitions
		      (variable-mark var '##compiler#local))
		  (not (get db var 'unknown)))
	     (let ((home (get db var 'home)))
	       (cond ((get db var 'local-value)
		      (put! db var 'unknown #t))
		     ((or (not home) (eq? here home))
		      (put! db var 'local-value val)	       )
		     (else (put! db var 'unknown #t)))))
	    (else (put! db var 'unknown #t)) ) )
    
    (define (ref var node)
      (collect! db var 'references node) )

    (define (quick-put! plist prop val)
      (set-cdr! plist (alist-cons prop val (cdr plist))) )

    ;; Return true if <id> directly or indirectly contains any of <other-ids>:
    (define (contains? id other-ids)
      (or (memq id other-ids)
	  (let ((clist (get db id 'contains)))
	    (and clist
		 (any (lambda (id2) (contains? id2 other-ids)) clist) ) ) ) )

    ;; Walk toplevel expression-node:
    (debugging 'p "analysis traversal phase...")
    (set! current-program-size 0)
    (walk node '() '() '() #f #f) 

    ;; Complete gathered database information:
    (debugging 'p "analysis gathering phase...")
    (set! current-analysis-database-size 0)    
    (##sys#hash-table-for-each
     (lambda (sym plist)
       (let ([unknown #f]
	     [value #f]
	     [local-value #f]
	     [pvalue #f]
	     [references '()]
	     [captured #f]
	     [call-sites '()]
	     [assigned #f]
	     [assigned-locally #f]
	     [undefined #f]
	     [global #f]
	     [rest-parameter #f] 
	     [nreferences 0]
	     [ncall-sites 0] )

         (set! current-analysis-database-size (fx+ current-analysis-database-size 1))
         
	 (for-each
	  (lambda (prop)
	    (case (car prop)
	      [(unknown) (set! unknown #t)]
	      [(references) 
	       (set! references (cdr prop))
	       (set! nreferences (length references)) ]
	      [(captured) (set! captured #t)]
	      [(potential-value) (set! pvalue (cdr prop))]
	      [(call-sites)
	       (set! call-sites (cdr prop))
	       (set! ncall-sites (length call-sites)) ]
	      [(assigned) (set! assigned #t)]
	      [(assigned-locally) (set! assigned-locally #t)]
	      [(undefined) (set! undefined #t)]
	      [(global) (set! global #t)]
	      [(value) (set! value (cdr prop))]
	      [(local-value) (set! local-value (cdr prop))]
	      [(rest-parameter) (set! rest-parameter #t)] ) )
	  plist)

	 (set! value (and (not unknown) value))
	 (set! local-value (and (not unknown) local-value))

	 ;; If this is the first analysis, register known local or potentially known global
	 ;;  lambda-value id's along with their names:
	 (when (and first-analysis 
		    (eq? '##core#lambda
			 (and-let* ([val (or value (and global pvalue))])
			   (node-class val) ) ) )
	   (set-real-name! (first (node-parameters (or value pvalue))) sym) )

	 ;; If this is the first analysis and the variable is global and has no references
	 ;;  and is hidden then issue warning:
	 (when (and first-analysis 
		    global
		    (null? references)
		    (not (variable-mark sym '##compiler#unused))
		    (not (variable-visible? sym))
		    (not (variable-mark sym '##compiler#constant)) )
	   (##sys#notice 
	    (sprintf "global variable `~S' is only locally visible and never used"
	      sym) ) )

 	 ;; Make 'boxed, if 'assigned & 'captured:
	 (when (and assigned captured)
	   (quick-put! plist 'boxed #t) )

	 ;; Make 'contractable, if it has a procedure as known value, has only one use
	 ;;  and one call-site and if the lambda has no free non-global variables 
	 ;;  or is an internal lambda. Make 'inlinable if
	 ;;  use/call count is not 1:
	 (cond (value
		(let ((valparams (node-parameters value)))
		  (when (and (eq? '##core#lambda (node-class value))
			     (or (not (second valparams))
				 (every 
				  (lambda (v) (get db v 'global))
				  (nth-value 0 (scan-free-variables value)) ) ) )
		    (if (and (= 1 nreferences) (= 1 ncall-sites))
			(quick-put! plist 'contractable #t)
			(quick-put! plist 'inlinable #t) ) ) ) )
	       (local-value
		;; Make 'inlinable, if it is declared local and has a value
		(let ((valparams (node-parameters local-value)))
		  (when (eq? '##core#lambda (node-class local-value))
		    (let-values (((vars hvars) (scan-free-variables local-value)))
		      (when (and (get db sym 'global)
				 (pair? hvars))
			(quick-put! plist 'hidden-refs #t))
		      (when (or (not (second valparams))
				(every 
				 (lambda (v) (get db v 'global)) 
				 vars))
			(quick-put! plist 'inlinable #t) ) ) ) ) )
	       ((variable-mark sym '##compiler#inline-global) =>
		(lambda (n)
		  (when (node? n)
		    (cond (assigned
			   (debugging
			    'i
			    "global inlining candidate was assigned and will not be inlined"
			    sym)
			   (mark-variable sym '##compiler#inline-global 'no))
			  (else
			   (let ((lparams (node-parameters n)))
			     (quick-put! plist 'inlinable #t)
			     (quick-put! plist 'local-value n))))))))

	 ;; Make 'collapsable, if it has a known constant value which is either collapsable or is only
	 ;;  referenced once and if no assignments are made:
	 (when (and value
		    ;; (not (assq 'assigned plist)) - If it has a known value, it's assigned just once!
		    (eq? 'quote (node-class value)) )
	   (let ((val (first (node-parameters value))))
	     (when (or (collapsable-literal? val)
		       (= 1 nreferences) )
	       (quick-put! plist 'collapsable #t) ) ) )
		
	 ;; If it has a known value that is a procedure, and if the number of call-sites is equal to the
	 ;;  number of references (does not escape), then make all formal parameters 'unused which are
	 ;;  never referenced or assigned (if no rest parameter exist):
	 ;;  - also marks the procedure as 'has-unused-parameters (if not in `callback-names')
	 ;;  - if the procedure is internal (a continuation) do NOT mark unused parameters.
	 ;;  - also: if procedure has rest-parameter and no unused params, mark f-id as 'explicit-rest.
	 (when value
	   (let ([has #f])
	     (when (and (eq? '##core#lambda (node-class value))
			(= nreferences ncall-sites) )
	       (let ([lparams (node-parameters value)])
		 (when (second lparams)
		   (decompose-lambda-list
		    (third lparams)
		    (lambda (vars argc rest)
		      (unless rest
			(for-each
			 (lambda (var)
			   (cond [(and (not (get db var 'references))
				       (not (get db var 'assigned)) )
				  (put! db var 'unused #t)
				  (set! has #t)
				  #t]
				 [else #f] ) )
			 vars) )
		      (cond [(and has (not (rassoc sym callback-names eq?)))
			     (put! db (first lparams) 'has-unused-parameters #t) ]
			    [rest
			     (set! explicitly-consed (cons rest explicitly-consed))
			     (put! db (first lparams) 'explicit-rest #t) ] ) ) ) ) ) ) ) )

	 ;; Make 'removable, if it has no references and is not assigned to, and if it 
	 ;; has either a value that does not cause any side-effects or if it is 'undefined:
	 (when (and (not assigned)
		    (null? references)
		    (or (and value
			     (if (eq? '##core#variable (node-class value))
				 (let ((varname (first (node-parameters value))))
				   (or (not (get db varname 'global))
				       (variable-mark varname '##core#always-bound)
				       (intrinsic? varname)))
				 (not (expression-has-side-effects? value db)) ))
			undefined) )
	   (quick-put! plist 'removable #t) )

	 ;; Make 'replacable, if it has a variable as known value and if either that variable has
	 ;;  a known value itself, or if it is not captured and referenced only once, the target and
	 ;;  the source are never assigned and the source is non-global or we are in block-mode:
	 ;;  - The target-variable is not allowed to be global.
	 ;;  - The variable that can be substituted for the current one is marked as 'replacing.
	 ;;    This is done to prohibit beta-contraction of the replacing variable (It wouldn't be there, if
	 ;;    it was contracted).
	 (when (and value (not global))
	   (when (eq? '##core#variable (node-class value))
	     (let* ([name (first (node-parameters value))]
		    [nrefs (get db name 'references)] )
	       (when (or (and (not (get db name 'unknown)) (get db name 'value))
			 (and (not (get db name 'captured))
			      nrefs
			      (= 1 (length nrefs))
			      (not assigned)
			      (not (get db name 'assigned)) 
			      (or (not (variable-visible? name))
				  (not (get db name 'global))) ) )
		 (quick-put! plist 'replacable name) 
		 (put! db name 'replacing #t) ) ) ) )

	 ;; Make 'replacable, if it has a known value of the form: '(lambda (<xvar>) (<kvar> <xvar>))' and
	 ;;  is an internally created procedure: (See above for 'replacing)
	 (when (and value (eq? '##core#lambda (node-class value)))
	   (let ([params (node-parameters value)])
	     (when (not (second params))
	       (let ([llist (third params)]
		     [body (first (node-subexpressions value))] )
		 (when (and (pair? llist) 
			    (null? (cdr llist))
			    (eq? '##core#call (node-class body)) )
		   (let ([subs (node-subexpressions body)])
		     (when (= 2 (length subs))
		       (let ([v1 (first subs)]
			     [v2 (second subs)] )
			 (when (and (eq? '##core#variable (node-class v1))
				    (eq? '##core#variable (node-class v2))
				    (eq? (first llist) (first (node-parameters v2))) )
			   (let ([kvar (first (node-parameters v1))])
			     (quick-put! plist 'replacable kvar)
			     (put! db kvar 'replacing #t) ) ) ) ) ) ) ) ) ) ) ) )

     db)

    ;; Set original program-size, if this is the first analysis-pass:
    (unless original-program-size
      (set! original-program-size current-program-size) )

    ;; return database
    db) )


;;; Collect unsafe global procedure calls that are assigned:

;;; Convert closures to explicit data structures (effectively flattens function-binding 
;   structure):

(define (perform-closure-conversion node db)
  (let ((direct-calls 0)
	(customizable '())
	(lexicals '()))

    (define (test sym item) (get db sym item))
  
    (define (register-customizable! var id)
      (set! customizable (lset-adjoin eq? customizable var)) 
      (put! db id 'customizable #t) )

    (define (register-direct-call! id)
      (set! direct-calls (add1 direct-calls))
      (set! direct-call-ids (lset-adjoin eq? direct-call-ids id)) )

    ;; Gather free-variable information:
    ;; (and: - register direct calls
    ;;       - update (by mutation) call information in "##core#call" nodes)
    (define (gather n here locals)
      (let ((subs (node-subexpressions n))
	    (params (node-parameters n)) )
	(case (node-class n)

	  ((##core#variable)
	   (let ((var (first params)))
	     (if (memq var lexicals)
		 (list var)
		 '())))

	  ((quote ##core#undefined ##core#proc ##core#primitive)
	   '())

	  ((let)
	   ;;XXX remove this test later, shouldn't be needed:
	   (when (pair? (cdr params)) (bomb "let-node has invalid format" params))
	   (let ((c (gather (first subs) here locals))
		 (var (first params)))
	     (append c (delete var (gather (second subs) here (cons var locals)) eq?))))

	  ((set!)
	   (let ((var (first params))
		 (c (gather (first subs) here locals)))
	     (if (memq var lexicals) 
		 (cons var c)
		 c)))

	  ((##core#call)
	   (let* ([fn (first subs)]
		  [mode (first params)]
		  [name (and (pair? (cdr params)) (second params))]
		  [varfn (eq? '##core#variable (node-class fn))] )
	     (node-parameters-set!
	      n
	      (cons mode
		    (if (or name varfn)
			(cons name
			      (if varfn
				  (let* ([varname (first (node-parameters fn))]
					 [val (and (not (test varname 'unknown)) 
						   (not (eq? 
							 'no
							 (variable-mark
							  varname '##compiler#inline)))
						   (or (test varname 'value)
						       (test varname 'local-value)))] )
				    (if (and val (eq? '##core#lambda (node-class val)))
					(let* ([params (node-parameters val)]
					       [llist (third params)]
					       [id (first params)]
					       [refs (test varname 'references)]
					       [sites (test varname 'call-sites)] 
					       [custom
						(and refs sites
						     (= (length refs) (length sites)) 
						     (test varname 'value)
						     (proper-list? llist) ) ] )
					  (when (and name 
						     (not (llist-match? llist (cdr subs))))
					    (quit
					     "~a: procedure `~a' called with wrong number of arguments" 
					     (source-info->line name)
					     (if (pair? name) (cadr name) name)))
					  (register-direct-call! id)
					  (when custom (register-customizable! varname id)) 
					  (list id custom) )
					'() ) )
				  '() ) )
			'() ) ) )
	     (concatenate (map (lambda (n) (gather n here locals)) subs) ) ))

	  ((##core#lambda ##core#direct_lambda)
	   (decompose-lambda-list
	    (third params)
	    (lambda (vars argc rest)
	      (let ((id (if here (first params) 'toplevel)))
		(fluid-let ((lexicals (append locals lexicals)))
		  (let ((c (delete-duplicates (gather (first subs) id vars) eq?)))
		    (put! db id 'closure-size (length c))
		    (put! db id 'captured-variables c)
		    (lset-difference eq? c locals vars)))))))
	
	  (else (concatenate (map (lambda (n) (gather n here locals)) subs)) ) ) ))

    ;; Create explicit closures:
    (define (transform n here closure)
      (let ((subs (node-subexpressions n))
	    (params (node-parameters n)) 
	    (class (node-class n)) )
	(case class

	  ((quote ##core#undefined ##core#proc) n)

	  ((##core#variable)
	   (let* ((var (first params))
		  (val (ref-var n here closure)) )
	     (if (test var 'boxed)
		 (make-node '##core#unbox '() (list val))
		 val) ) )

	  ((if ##core#call ##core#inline ##core#inline_allocate ##core#callunit 
	       ##core#inline_ref ##core#inline_update 
	       ##core#switch ##core#cond ##core#direct_call ##core#recurse ##core#return 
	       ##core#inline_loc_ref
	       ##core#inline_loc_update)
	   (make-node (node-class n) params (maptransform subs here closure)) )

	  ((let)
	   (let* ([var (first params)]
		  [boxedvar (test var 'boxed)]
		  [boxedalias (gensym var)] )
	     (if boxedvar
		 (make-node 
		  'let (list boxedalias)
		  (list (transform (first subs) here closure)
			(make-node
			 'let (list var)
			 (list (make-node '##core#box '() (list (varnode boxedalias)))
			       (transform (second subs) here closure) ) ) ) )
		 (make-node
		  'let params
		  (maptransform subs here closure) ) ) ) )

	  ((##core#lambda ##core#direct_lambda)
	   (let ([llist (third params)])
	     (decompose-lambda-list
	      llist
	      (lambda (vars argc rest)
		(let* ([boxedvars (filter (lambda (v) (test v 'boxed)) vars)]
		       [boxedaliases (map cons boxedvars (map gensym boxedvars))]
		       [cvar (gensym 'c)]
		       [id (if here (first params) 'toplevel)]
		       [capturedvars (or (test id 'captured-variables) '())]
		       [csize (or (test id 'closure-size) 0)] 
		       [info (and emit-closure-info (second params) (pair? llist))] )
		  ;; If rest-parameter is boxed: mark it as 'boxed-rest
		  ;;  (if we don't do this than preparation will think the (boxed) alias
		  ;;  of the rest-parameter is never used)
		  (and-let* ([rest]
			     [(test rest 'boxed)]
			     [rp (test rest 'rest-parameter)] )
		    (put! db (cdr (assq rest boxedaliases)) 'boxed-rest #t) )
		  (make-node
		   '##core#closure (list (+ csize (if info 2 1)))
		   (cons
		    (make-node
		     class
		     (list id
			   (second params)
			   (cons 
			    cvar
			    (build-lambda-list
			     (map (lambda (v)
				    (cond ((assq v boxedaliases) => cdr)
					  (else v) ) )
				  vars)
			     argc
			     (cond ((and rest (assq rest boxedaliases)) => cdr)
				   (else rest) ) ) )
			   (fourth params) )
		     (list (let ((body (transform (car subs) cvar capturedvars)))
			     (if (pair? boxedvars)
				 (fold-right
				  (lambda (alias val body)
				    (make-node 'let (list alias) (list val body)))
				  body
				  (unzip1 boxedaliases)
				  (map (lambda (a)
					 (make-node '##core#box '() (list (varnode (cdr a)))))
				       boxedaliases) )
				 body) ) ) )
		    (let ((cvars (map (lambda (v) (ref-var (varnode v) here closure))
				      capturedvars) ) )
		      (if info
			  (append 
			   cvars
			   (list 
			    (qnode 
			     (##sys#make-lambda-info
			      (->string (cons (or (real-name id) '?)
					      (cdr llist) )))))) ; this is not always correct, due to optimizations
			  cvars) ) ) ) ) ) ) ) )

	  ((set!)
	   (let* ([var (first params)]
		  [val (first subs)]
		  [cval (node-class val)]
		  [immf (or (and (eq? 'quote cval) (immediate? (first (node-parameters val))))
			    (eq? '##core#undefined cval) ) ] )
	     (cond ((posq var closure)
		    => (lambda (i)
			 (if (test var 'boxed)
			     (make-node
			      (if immf '##core#updatebox_i '##core#updatebox)
			      '()
			      (list (make-node '##core#ref (list (add1 i)) (list (varnode here)))
				    (transform val here closure) ) )
			     ;; Is the following actually used???
			     (make-node
			      (if immf '##core#update_i '##core#update)
			      (list (add1 i))
			      (list (varnode here)
				    (transform val here closure) ) ) ) ) )
		   ((test var 'boxed)
		    (make-node
		     (if immf '##core#updatebox_i '##core#updatebox)
		     '()
		     (list (varnode var)
			   (transform val here closure) ) ) )
		   (else (make-node
			  'set! (list var)
			  (list (transform val here closure) ) ) ) ) ) )

	  ((##core#primitive) 
	   (make-node
	    '##core#closure (list (if emit-closure-info 2 1))
	    (cons (make-node '##core#proc (list (car params) #t) '())
		  (if emit-closure-info
		      (list (qnode (##sys#make-lambda-info (car params))))
		      '() ) ) ) )

	  (else (bomb "bad node (closure2)")) ) ) )

    (define (maptransform xs here closure)
      (map (lambda (x) (transform x here closure)) xs) )
  
    (define (ref-var n here closure)
      (let ((var (first (node-parameters n))))
	(cond ((posq var closure) 
	       => (lambda (i) 
		    (make-node '##core#ref (list (+ i 1)) 
			       (list (varnode here)) ) ) )
	      (else n) ) ) )

    (debugging 'p "closure conversion gathering phase...")
    (gather node #f '())
    (when (pair? customizable)
      (debugging 'o "customizable procedures" customizable))
    (debugging 'p "closure conversion transformation phase...")
    (let ((node2 (transform node #f #f)))
      (unless (zero? direct-calls)
	(debugging 'o "calls to known targets" direct-calls))
      node2) ) )


;;; Do some preparations before code-generation can commence:

(define-record-type lambda-literal
  (make-lambda-literal id external arguments argument-count rest-argument temporaries
		       unboxed-temporaries callee-signatures allocated directly-called
		       closure-size looping customizable rest-argument-mode body direct)
  lambda-literal?
  (id lambda-literal-id)			       ; symbol
  (external lambda-literal-external)		       ; boolean
  (arguments lambda-literal-arguments)		       ; (symbol ...)
  (argument-count lambda-literal-argument-count)       ; integer
  (rest-argument lambda-literal-rest-argument)	       ; symbol | #f
  (temporaries lambda-literal-temporaries)	       ; integer
  (unboxed-temporaries lambda-literal-unboxed-temporaries) ; ((sym . utype) ...)
  (callee-signatures lambda-literal-callee-signatures) ; (integer ...)
  (allocated lambda-literal-allocated)		       ; integer
  (directly-called lambda-literal-directly-called)     ; boolean
  (closure-size lambda-literal-closure-size)	       ; integer
  (looping lambda-literal-looping)		       ; boolean
  (customizable lambda-literal-customizable)	       ; boolean
  (rest-argument-mode lambda-literal-rest-argument-mode) ; #f | LIST | NONE
  (body lambda-literal-body)				 ; expression
  (direct lambda-literal-direct))			 ; boolean
  
(define (prepare-for-code-generation node db)
  (let ((literals '())
        (literal-count 0)
	(lambda-info-literals '())
        (lambda-info-literal-count 0)
        ;; Use analysis db as optimistic heuristic for procedure table size
        (lambda-table (make-vector (fx* (fxmax current-analysis-database-size 1) 3) '()))
        (temporaries 0)
	(ubtemporaries '())
        (allocated 0)
	(looping 0)
        (signatures '()) 
	(fastinits 0) 
	(fastrefs 0) 
	(fastsets 0) )

    (define (walk-var var e e-count sf)
      (cond [(posq var e)
             => (lambda (i)
                  (make-node '##core#local (list (fx- e-count (fx+ i 1))) '()))]
	    [(keyword? var) (make-node '##core#literal (list (literal var)) '())]
	    [else (walk-global var sf)] ) )

    (define (walk-global var sf)
      (let* ([safe (or sf 
		       no-bound-checks
		       unsafe
		       (variable-mark var '##compiler#always-bound)
		       (intrinsic? var))]
	     [blockvar (and (get db var 'assigned)
			    (not (variable-visible? var)))])
	(when blockvar (set! fastrefs (add1 fastrefs)))
	(make-node
	 '##core#global
	 (list (if blockvar
		   (blockvar-literal var)
		   (literal var) )
	       safe
	       blockvar
	       var)
	 '() ) ) )

    (define (walk n e e-count here boxes)
      (let ((subs (node-subexpressions n))
	    (params (node-parameters n))
	    (class (node-class n)) )
	(case class

	  ((##core#undefined ##core#proc) n)

	  ((##core#variable) 
	   (walk-var (first params) e e-count #f) )

	  ((##core#direct_call)
	   (set! allocated (+ allocated (fourth params)))
	   (make-node class params (mapwalk subs e e-count here boxes)) )

	  ((##core#inline_allocate)
	   (set! allocated (+ allocated (second params)))
	   (make-node class params (mapwalk subs e e-count here boxes)) )

	  ((##core#inline_ref)
	   (set! allocated (+ allocated (words (estimate-foreign-result-size (second params)))))
	   (make-node class params '()) )

	  ((##core#inline_loc_ref)
	   (set! allocated (+ allocated (words (estimate-foreign-result-size (first params)))))
	   (make-node class params (mapwalk subs e e-count here boxes)) )

	  ((##core#closure) 
	   (set! allocated (+ allocated (first params) 1))
	   (make-node '##core#closure params (mapwalk subs e e-count here boxes)) )

	  ((##core#box)
	   (set! allocated (+ allocated 2))
	   (make-node '##core#box params (list (walk (first subs) e e-count here boxes))) )

	  ((##core#updatebox)
	   (let* ([b (first subs)]
		  [subs (mapwalk subs e e-count here boxes)] )
	     (make-node
	      (cond [(and (eq? '##core#variable (node-class b))
			  (memq (first (node-parameters b)) boxes) )
		     (set! fastinits (add1 fastinits))
		     '##core#updatebox_i]
		    [else class] )
	      '()
	      subs) ) )

	  ((##core#lambda ##core#direct_lambda) 
	   (let ((temps temporaries)
		 (ubtemps ubtemporaries)
		 (sigs signatures)
		 (lping looping)
		 (alc allocated) 
		 (direct (eq? class '##core#direct_lambda)) )
	     (set! temporaries 0)
	     (set! ubtemporaries '())
	     (set! allocated 0)
	     (set! signatures '())
	     (set! looping 0)
	     (decompose-lambda-list
	      (third params)
	      (lambda (vars argc rest)
		(let* ([id (first params)]
		       [rest-mode
			(and rest
			     (let ([rrefs (get db rest 'references)])
			       (cond [(get db rest 'assigned) 'list]
				     [(and (not (get db rest 'boxed-rest)) (or (not rrefs) (null? rrefs))) 'none] 
				     [else (get db rest 'rest-parameter)] ) ) ) ]
		       [body (walk 
			      (car subs)
			      (##sys#fast-reverse (if (eq? 'none rest-mode)
                                                      (butlast vars)
                                                      vars))
                              (if (eq? 'none rest-mode)
				  (fx- (length vars) 1)
				  (length vars))
			      id
			      '()) ] )
		  (when (eq? rest-mode 'none)
		    (debugging 'o "unused rest argument" rest id))
		  (when (and direct rest)
		    (bomb "bad direct lambda" id allocated rest) )
		  (##sys#hash-table-set!
                   lambda-table
                   id
                   (make-lambda-literal
                    id
                    (second params)
                    vars
                    argc
                    rest
                    (add1 temporaries)
                    ubtemporaries
                    signatures
                    allocated
                    (or direct (memq id direct-call-ids))
                    (or (get db id 'closure-size) 0)
                    (and (not rest)
                         (> looping 0)
                         (begin
                           (debugging 'o "identified direct recursive calls" id looping)
                           #t) )
                    (or direct (get db id 'customizable))
                    rest-mode
                    body
                    direct) )
		  (set! looping lping)
		  (set! temporaries temps)
		  (set! ubtemporaries ubtemps)
		  (set! allocated alc)
		  (set! signatures sigs)
		  (make-node '##core#proc (list (first params)) '()) ) ) ) ) )

	  ((let)
	   (let* ([var (first params)]
		  [val (first subs)] 
		  [boxvars (if (eq? '##core#box (node-class val)) (list var) '())] )
	     (set! temporaries (add1 temporaries))
	     (make-node
	      '##core#bind (list 1)	; is actually never used with more than 1 variable
	      (list (walk val e e-count here boxes)
		    (walk (second subs)
                          (append (##sys#fast-reverse params) e) (fx+ e-count 1)
                          here (append boxvars boxes)) ) ) ) )

	  ((##core#let_unboxed)
	   (let* ((var (first params))
		  (val (first subs)) )
	     (set! ubtemporaries (alist-cons var (second params) ubtemporaries))
	     (make-node
	      '##core#let_unboxed params
	      (list (walk val e e-count here boxes)
		    (walk (second subs) e e-count here boxes) ) ) ) )

	  ((set!)
	   (let ([var (first params)]
		 [val (first subs)] )
	     (cond ((posq var e)
		    => (lambda (i)
                         (make-node '##core#setlocal
                                    (list (fx- e-count (fx+ i 1)))
                                    (list (walk val e e-count here boxes)) ) ) )
		   (else
		    (let* ([cval (node-class val)]
			   [blockvar (not (variable-visible? var))]
			   [immf (or (and (eq? cval 'quote) (immediate? (first (node-parameters val))))
				     (eq? '##core#undefined cval) ) ] )
		      (when blockvar (set! fastsets (add1 fastsets)))
		      (make-node
		       (if immf '##core#setglobal_i '##core#setglobal)
		       (list (if blockvar
				 (blockvar-literal var)
				 (literal var) )
			     blockvar
			     var)
		       (list (walk (car subs) e e-count here boxes)) ) ) ) ) ) )

	  ((##core#call) 
	   (let ([len (length (cdr subs))])
	     (set! signatures (lset-adjoin = signatures len)) 
	     (when (and (>= (length params) 3) (eq? here (third params)))
	       (set! looping (add1 looping)) )
	     (make-node class params (mapwalk subs e e-count here boxes)) ) )

	  ((##core#recurse)
	   (when (first params) (set! looping (add1 looping)))
	   (make-node class params (mapwalk subs e e-count here boxes)) )

	  ((quote)
	   (let ((c (first params)))
	     (cond ((and (fixnum? c) (not (big-fixnum? c)))
		    (immediate-literal c) )
		   ((number? c)
		    (cond ((eq? 'fixnum number-type)
			   (cond ((and (integer? c) (not (big-fixnum? c)))
				  (warning 
				   (sprintf 
				       "coerced inexact literal number `~S' to fixnum ~S" 
				     c (inexact->exact c)))
				  (immediate-literal (inexact->exact c)) )
				 (else (quit "cannot coerce inexact literal `~S' to fixnum" c)) ) )
			  (else (make-node '##core#literal (list (literal c)) '())) ) )
		   ((immediate? c) (immediate-literal c))
		   (else (make-node '##core#literal (list (literal c)) '())) ) ) )

	  ((if ##core#cond)
	   (let* ((test (walk (first subs) e e-count here boxes))
		  (a0 allocated)
		  (x1 (walk (second subs) e e-count here boxes))
		  (a1 allocated)
		  (x2 (walk (third subs) e e-count here boxes)))
	     (set! allocated (+ a0 (max (- allocated a1) (- a1 a0))))
	     (make-node class params (list test x1 x2))))

	  ((##core#switch)
	   (let* ((exp (walk (first subs) e e-count here boxes))
		  (a0 allocated))
	     (make-node
	      class
	      params
	      (cons 
	       exp
	       (let loop ((j (first params)) (subs (cdr subs)) (ma 0))
		 (set! allocated a0)
		 (if (zero? j)
		     (let ((def (walk (car subs) e e-count here boxes)))
		       (set! allocated (+ a0 (max ma (- allocated a0))))
		       (list def))
		     (let* ((const (walk (car subs) e e-count here boxes))
			    (body (walk (cadr subs) e e-count here boxes)))
		       (cons* 
			const body
			(loop (sub1 j) (cddr subs) (max (- allocated a0) ma))))))))))

	  (else (make-node class params (mapwalk subs e e-count here boxes)) ) ) ) )
    
    (define (mapwalk xs e e-count here boxes)
      (map (lambda (x) (walk x e e-count here boxes)) xs) )

    (define (literal x)
      (cond [(immediate? x) (immediate-literal x)]
            ;; Fixnums that don't fit in 32 bits are treated as non-immediates,
            ;; that's why we do the (apparently redundant) C_blockp check here.
	    ((and (##core#inline "C_blockp" x) (##core#inline "C_lambdainfop" x))
	     (let ((i lambda-info-literal-count))
	       (set! lambda-info-literals (cons x lambda-info-literals))
               (set! lambda-info-literal-count (add1 lambda-info-literal-count))
	       (vector i) ) )
            [(posv x literals) => (lambda (p) (fx- literal-count (fx+ p 1)))]
	    [else (new-literal x)] ) )

    (define (new-literal x)
      (let ([i literal-count])
	(set! literals (cons x literals))
        (set! literal-count (add1 literal-count))
	i) )

    (define (blockvar-literal var)
      (cond
       ((list-index (lambda (lit) 
                      (and (block-variable-literal? lit)
                           (eq? var (block-variable-literal-name lit)) ) )
                    literals)
        => (lambda (p) (fx- literal-count (fx+ p 1))))
       (else (new-literal (make-block-variable-literal var))) ) )
    
    (define (immediate-literal x)
      (if (eq? (void) x)
	  (make-node '##core#undefined '() '())
	  (make-node '##core#immediate
		     (cond ((fixnum? x) `(fix ,x))
			   ((boolean? x) `(bool ,x))
			   ((char? x) `(char ,x))
			   ((null? x) '(nil))
			   ((eof-object? x) '(eof))
			   (else (bomb "bad immediate (prepare)")) )
		     '() ) ) )
    
    (debugging 'p "preparation phase...")
    (let ((node2 (walk node '() 0 #f '())))
      (when (positive? fastinits)
	(debugging 'o "fast box initializations" fastinits))
      (when (positive? fastrefs)
	(debugging 'o "fast global references" fastrefs))
      (when (positive? fastsets)
	(debugging 'o "fast global assignments" fastsets))
      (values node2 (##sys#fast-reverse literals)
              (##sys#fast-reverse lambda-info-literals) lambda-table) ) ) )
