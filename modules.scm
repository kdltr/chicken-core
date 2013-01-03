;;;; modules.scm - module-system support
;
; Copyright (c) 2011-2013, The Chicken Team
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
  (unit modules)
  (disable-interrupts)
  (fixnum)
  (hide lookup merge-se module-indirect-exports)
  (not inline ##sys#alias-global-hook))

(include "common-declarations.scm")

(define-syntax d (syntax-rules () ((_ . _) (void))))

(define-alias dd d)
(define-alias dm d)
(define-alias dx d)

#+debugbuild
(define (map-se se)
  (map (lambda (a) 
	 (cons (car a) (if (symbol? (cdr a)) (cdr a) '<macro>)))
       se))

(define-inline (getp sym prop)
  (##core#inline "C_i_getprop" sym prop #f))

(define-inline (putp sym prop val)
  (##core#inline_allocate ("C_a_i_putprop" 8) sym prop val))


;;; Support definitions

;; duoplicates code in the hope of being inlined
(define (lookup id se)
  (cond ((##core#inline "C_u_i_assq" id se) => cdr)
	((getp id '##core#macro-alias))
	(else #f)))

#+debugbuild
(define (map-se se)
  (map (lambda (a) 
	 (cons (car a) (if (symbol? (cdr a)) (cdr a) '<macro>)))
       se))


;;; low-level module support

(define ##sys#current-module (make-parameter #f))
(define ##sys#module-alias-environment (make-parameter '()))

(declare 
  (hide make-module module? %make-module
	module-name module-vexports module-sexports
	set-module-vexports! set-module-sexports!
	module-export-list set-module-export-list! 
	module-defined-list set-module-defined-list!
	module-import-forms set-module-import-forms!
	module-meta-import-forms set-module-meta-import-forms!
	module-exist-list set-module-exist-list!
	module-meta-expressions set-module-meta-expressions!
	module-defined-syntax-list set-module-defined-syntax-list!
	module-saved-environments set-module-saved-environments!
	module-iexports set-module-iexports!))

(define-record-type module
  (%make-module name export-list defined-list exist-list defined-syntax-list
		undefined-list import-forms meta-import-forms meta-expressions 
		vexports sexports iexports saved-environments) 
  module?
  (name module-name)			; SYMBOL
  (export-list module-export-list set-module-export-list!) ; (SYMBOL | (SYMBOL ...) ...)
  (defined-list module-defined-list set-module-defined-list!) ; ((SYMBOL . VALUE) ...)    - *exported* value definitions
  (exist-list module-exist-list set-module-exist-list!)	      ; (SYMBOL ...)    - only for checking refs to undef'd
  (defined-syntax-list module-defined-syntax-list set-module-defined-syntax-list!) ; ((SYMBOL . VALUE) ...)
  (undefined-list module-undefined-list set-module-undefined-list!) ; ((SYMBOL WHERE1 ...) ...)
  (import-forms module-import-forms set-module-import-forms!)	    ; (SPEC ...)
  (meta-import-forms module-meta-import-forms set-module-meta-import-forms!)	    ; (SPEC ...)
  (meta-expressions module-meta-expressions set-module-meta-expressions!) ; (EXP ...)
  (vexports module-vexports set-module-vexports!)	      ; ((SYMBOL . SYMBOL) ...)
  (sexports module-sexports set-module-sexports!)	      ; ((SYMBOL SE TRANSFORMER) ...)
  (iexports module-iexports set-module-iexports!)	      ; ((SYMBOL . SYMBOL) ...)
  ;; for csi's ",m" command, holds (<env> . <macroenv>)
  (saved-environments module-saved-environments set-module-saved-environments!))

(define ##sys#module-name module-name)

(define (##sys#module-exports m)
  (values 
   (module-export-list m)
   (module-vexports m)
   (module-sexports m)))

(define (make-module name explist vexports sexports iexports)
  (%make-module name explist '() '() '() '() '() '() '() vexports sexports iexports #f))

(define (##sys#register-module-alias alias name)
  (##sys#module-alias-environment
    (cons (cons alias name) (##sys#module-alias-environment))))

(define (##sys#with-module-aliases bindings thunk)
  (parameterize ((##sys#module-alias-environment
		  (append
		   (map (lambda (b) (cons (car b) (cadr b))) bindings)
		   (##sys#module-alias-environment))))
    (thunk)))

(define (##sys#resolve-module-name name loc)
  (let loop ((n name) (done '()))
    (cond ((assq n (##sys#module-alias-environment)) =>
	   (lambda (a)
	     (let ((n2 (cdr a)))
	       (if (memq n2 done)
		   (error loc "module alias refers to itself" name)
		   (loop n2 (cons n2 done))))))
	  (else n))))

(define (##sys#find-module name #!optional (err #t) loc)
  (cond ((assq name ##sys#module-table) => cdr)
	(err (error loc "module not found" name))
	(else #f)))

(define ##sys#switch-module
  (let ((saved-default-envs #f))
    (lambda (mod)
      (let ((now (cons (##sys#current-environment) (##sys#macro-environment))))
	(cond ((##sys#current-module) =>
	       (lambda (m)
		 (set-module-saved-environments! m now)))
	      (else 
	       (set! saved-default-envs now)))
	(let ((saved (if mod (module-saved-environments mod) saved-default-envs)))
	  (when saved
	    (##sys#current-environment (car saved))
	    (##sys#macro-environment (cdr saved)))
	  (##sys#current-module mod))))))

(define (##sys#add-to-export-list mod exps)
  (let ((xl (module-export-list mod)))
    (if (eq? xl #t)
	(let ((el (module-exist-list mod))
	      (me (##sys#macro-environment))
	      (sexps '()))
	  (for-each
	   (lambda (exp)
	     (cond ((assq exp me) =>
		    (lambda (a)
		      (set! sexps (cons a sexps))))))
	   exps)
	  (set-module-sexports! mod (append sexps (module-sexports mod)))
	  (set-module-exist-list! mod (append el exps)))
	(set-module-export-list! mod (append xl exps)))))

(define (##sys#toplevel-definition-hook sym mod exp val) #f)

(define (##sys#register-meta-expression exp)
  (and-let* ((mod (##sys#current-module)))
    (set-module-meta-expressions! mod (cons exp (module-meta-expressions mod)))))

(define (check-for-redef sym env senv)
  (and-let* ((a (assq sym env)))
    (##sys#warn "redefinition of imported value binding" sym) )
  (and-let* ((a (assq sym senv)))
    (##sys#warn "redefinition of imported syntax binding" sym)))

(define (##sys#register-export sym mod)
  (when mod
    (let ((exp (or (eq? #t (module-export-list mod))
		   (##sys#find-export sym mod #t)))
	  (ulist (module-undefined-list mod)))
      (##sys#toplevel-definition-hook	; in compiler, hides unexported bindings
       (##sys#module-rename sym (module-name mod)) 
       mod exp #f)
      (and-let* ((a (assq sym ulist)))
	(set-module-undefined-list! mod (##sys#delq a ulist)))
      (check-for-redef sym (##sys#current-environment) (##sys#macro-environment))
      (set-module-exist-list! mod (cons sym (module-exist-list mod)))
      (when exp
	(dm "defined: " sym)
	(set-module-defined-list! 
	 mod
	 (cons (cons sym #f)
	       (module-defined-list mod)))))) )

(define (##sys#register-syntax-export sym mod val)
  (when mod
    (let ((exp (or (eq? #t (module-export-list mod))
		   (##sys#find-export sym mod #t)))
	  (ulist (module-undefined-list mod))
	  (mname (module-name mod)))
      (when (assq sym ulist)	    
	(##sys#warn "use of syntax precedes definition" sym)) ;XXX could report locations
      (check-for-redef sym (##sys#current-environment) (##sys#macro-environment))
      (dm "defined syntax: " sym)
      (when exp
	(set-module-defined-list! 
	 mod
	 (cons (cons sym val)
	       (module-defined-list mod))) )
      (set-module-defined-syntax-list! 
       mod
       (cons (cons sym val) (module-defined-syntax-list mod))))))

(define (##sys#register-undefined sym mod where)
  (when mod
    (let ((ul (module-undefined-list mod)))
      (cond ((assq sym ul) =>
	     (lambda (a)
	       (when (and where (not (memq where (cdr a))))
		 (set-cdr! a (cons where (cdr a))))))
	    (else
	     (set-module-undefined-list!
	      mod
	      (cons (cons sym (if where (list where) '())) ul)))))))

(define (##sys#register-module name explist #!optional (vexports '()) (sexports '()))
  (let ((mod (make-module name explist vexports sexports '())))
    (set! ##sys#module-table (cons (cons name mod) ##sys#module-table))
    mod) )

(define (##sys#mark-imported-symbols se)
  (for-each
   (lambda (imp)
     (when (and (symbol? (cdr imp)) (not (eq? (car imp) (cdr imp))))
       (dm `(MARKING: ,(cdr imp)))
       (putp (cdr imp) '##core#aliased #t)))
   se))

(define (module-indirect-exports mod)
  (let ((exports (module-export-list mod))
	(mname (module-name mod))
	(dlist (module-defined-list mod)))
    (define (indirect? id)
      (let loop ((exports exports))
	(and (not (null? exports))
	     (or (and (pair? (car exports))
		      (memq id (cdar exports)))
		 (loop (cdr exports))))))
    (define (warn msg id)
      (##sys#warn
       (string-append msg " in module `" (symbol->string mname) "'")
       id))
    (if (eq? #t exports)
	'()
	(let loop ((exports exports))	; walk export list
	  (cond ((null? exports) '())
		((symbol? (car exports)) (loop (cdr exports))) ; normal export
		(else
		 (let loop2 ((iexports (cdar exports))) ; walk indirect exports for a given entry
		   (cond ((null? iexports) (loop (cdr exports)))
			 ((assq (car iexports) (##sys#macro-environment))
			  (warn "indirect export of syntax binding" (car iexports))
			  (loop2 (cdr iexports)))
			 ((assq (car iexports) dlist) => ; defined in current module?
			  (lambda (a) 
			    (cons 
			     (cons 
			      (car iexports)
			      (or (cdr a) (##sys#module-rename (car iexports) mname)))
			     (loop2 (cdr iexports)))))
			 ((assq (car iexports) (##sys#current-environment)) =>
			  (lambda (a)	; imported in current env.
			    (cond ((symbol? (cdr a)) ; not syntax
				   (cons (cons (car iexports) (cdr a)) (loop2 (cdr iexports))) )
				  (else
				   (warn "indirect reexport of syntax" (car iexports))
				   (loop2 (cdr iexports))))))
			 (else 
			  (warn "indirect export of unknown binding" (car iexports))
			  (loop2 (cdr iexports)))))))))))

(define (merge-se . ses)		; later occurrences take precedence to earlier ones
  (let ((se (apply append ses)))
    (dm "merging " (length ses) " se's with total length of " (length se))
    (let ((se2
	   (let loop ((se se))
	     (cond ((null? se) '())
		   ((assq (caar se) (cdr se)) (loop (cdr se)))
		   (else (cons (car se) (loop (cdr se))))))))
      (dm "  merged has length " (length se2))
      se2)))

(define (##sys#compiled-module-registration mod)
  (let ((dlist (module-defined-list mod))
	(mname (module-name mod))
	(ifs (module-import-forms mod))
	(sexports (module-sexports mod))
	(mifs (module-meta-import-forms mod)))
    `(,@(if (pair? ifs) `((eval '(import ,@(##sys#strip-syntax ifs)))) '())
      ,@(if (pair? mifs) `((import ,@(##sys#strip-syntax mifs))) '())
      ,@(##sys#fast-reverse (map ##sys#strip-syntax (module-meta-expressions mod)))
      (##sys#register-compiled-module
       ',(module-name mod)
       (list
	,@(map (lambda (ie)
		 (if (symbol? (cdr ie))
		     `'(,(car ie) . ,(cdr ie))
		     `(list ',(car ie) '() ,(cdr ie))))
	       (module-iexports mod)))
       ',(module-vexports mod)
       (list 
	,@(map (lambda (sexport)
		 (let* ((name (car sexport))
			(a (assq name dlist)))
		   (cond ((pair? a) 
			  `(cons ',(car sexport) ,(##sys#strip-syntax (cdr a))))
			 (else
			  (dm "re-exported syntax" name mname)
			  `',name))))
	       sexports))
       (list 
	,@(if (null? sexports)
	      '() 			; no syntax exported - no more info needed
	      (let loop ((sd (module-defined-syntax-list mod)))
		(cond ((null? sd) '())
		      ((assq (caar sd) sexports) (loop (cdr sd)))
		      (else
		       (let ((name (caar sd)))
			 (cons `(cons ',(caar sd) ,(##sys#strip-syntax (cdar sd)))
			       (loop (cdr sd)))))))))))))

(define (##sys#register-compiled-module name iexports vexports sexports #!optional
					(sdefs '()))
  (define (find-reexport name)
    (let ((a (assq name (##sys#macro-environment))))
      (if (and a (pair? (cdr a)))
	  a
	  (##sys#error
	   'import "cannot find implementation of re-exported syntax"
	   name))))
  (let* ((sexps
	  (map (lambda (se)
		 (if (symbol? se)
		     (find-reexport se)
		     (list (car se) #f (##sys#ensure-transformer (cdr se) (car se)))))
	       sexports))
	 (iexps 
	  (map (lambda (ie)
		 (if (pair? (cdr ie))
		     (list (car ie) (cadr ie) (##sys#ensure-transformer (caddr ie) (car ie)))
		     ie))
	       iexports))
	 (nexps
	  (map (lambda (ne)
		 (list (car ne) #f (##sys#ensure-transformer (cdr ne) (car ne))))
	       sdefs))
	 (mod (make-module name '() vexports sexps iexps))
	 (senv (merge-se 
		(##sys#macro-environment)
		(##sys#current-environment)
		iexps vexports sexps nexps)))
    (##sys#mark-imported-symbols iexps)
    (for-each
     (lambda (sexp)
       (set-car! (cdr sexp) senv))
     sexps)
    (for-each
     (lambda (iexp)
       (when (pair? (cdr iexp))
	 (set-car! (cdr iexp) senv)))
     iexps)
    (for-each
     (lambda (nexp)
       (set-car! (cdr nexp) senv))
     nexps)
    (set! ##sys#module-table (cons (cons name mod) ##sys#module-table)) 
    mod))

(define (##sys#primitive-alias sym)
  (let ((palias 
	 (##sys#string->symbol 
	  (##sys#string-append "#%" (##sys#slot sym 1)))))
    (putp palias '##core#primitive sym)
    palias))

(define (##sys#register-primitive-module name vexports #!optional (sexports '()))
  (let* ((me (##sys#macro-environment))
	 (mod (make-module 
	       name '()
	       (map (lambda (ve)
		      (if (symbol? ve)
			  (cons ve (##sys#primitive-alias ve))
			  ve))
		    vexports)
	       (map (lambda (se)
		      (if (symbol? se)
			  (or (assq se me)
			      (##sys#error
			       "unknown syntax referenced while registering module" 
			       se name))
			  se))
		    sexports)
	       '())))
    (set-module-saved-environments!
     mod
     (cons (merge-se (##sys#current-environment)
		     (module-vexports mod)
		     (module-sexports mod))
	   (##sys#macro-environment)))
    (set! ##sys#module-table (cons (cons name mod) ##sys#module-table)) 
    mod))

(define (##sys#find-export sym mod indirect)
  (let ((exports (module-export-list mod)))
    (let loop ((xl (if (eq? #t exports) (module-exist-list mod) exports)))
      (cond ((null? xl) #f)
	    ((eq? sym (car xl)))
	    ((pair? (car xl))
	     (or (eq? sym (caar xl))
		 (and indirect (memq sym (cdar xl)))
		 (loop (cdr xl))))
	    (else (loop (cdr xl)))))))

(define ##sys#finalize-module 
  (let ((display display)
	(write-char write-char))
    (lambda (mod)
      (let* ((explist (module-export-list mod))
	     (name (module-name mod))
	     (dlist (module-defined-list mod))
	     (elist (module-exist-list mod))
	     (missing #f)
	     (sdlist (map (lambda (sym) (assq (car sym) (##sys#macro-environment)))
			  (module-defined-syntax-list mod)))
	     (sexports
	      (if (eq? #t explist)
		  (merge-se (module-sexports mod) sdlist)
		  (let loop ((me (##sys#macro-environment)))
		    (cond ((null? me) '())
			  ((##sys#find-export (caar me) mod #f)
			   (cons (car me) (loop (cdr me))))
			  (else (loop (cdr me)))))))
	     (vexports
	      (let loop ((xl (if (eq? #t explist) elist explist)))
		(if (null? xl)
		    '()
		    (let* ((h (car xl))
			   (id (if (symbol? h) h (car h))))
		      (if (assq id sexports) 
			  (loop (cdr xl))
			  (cons 
			   (cons 
			    id
			    (let ((def (assq id dlist)))
			      (if (and def (symbol? (cdr def))) 
				  (cdr def)
				  (let ((a (assq id (##sys#current-environment))))
				    (cond ((and a (symbol? (cdr a))) 
					   (dm "reexporting: " id " -> " (cdr a))
					   (cdr a)) 
					  ((not def)
					   (set! missing #t)
					   (##sys#warn 
					    (string-append 
					     "exported identifier of module `" 
					     (symbol->string name)
					     "' has not been defined")
					    id)
					   #f)
					  (else (##sys#module-rename id name)))))))
			   (loop (cdr xl)))))))))
	(for-each
	 (lambda (u)
	   (let* ((where (cdr u))
		  (u (car u)))
	     (unless (memq u elist)
	       (let ((out (open-output-string)))
		 (set! missing #t)
		 (display "reference to possibly unbound identifier `" out)
		 (display u out)
		 (write-char #\' out)
		 (when (pair? where)
		   (display " in:" out)
		   (for-each
		    (lambda (sym)
		      (display "\nWarning:    " out)
		      (display sym out))
		    where))
		 (and-let* ((a (getp u '##core#db)))
		   (cond ((= 1 (length a))
			  (display "\nWarning:    suggesting: `(import " out)
			  (display (cadar a) out)
			  (display ")'" out))
			 (else
			  (display "\nWarning:    suggesting one of:" out)
			  (for-each
			   (lambda (a)
			     (display "\nWarning:    (import " out)
			     (display (cadr a) out)
			     (write-char #\) out))
			   a))))
		 (##sys#warn (get-output-string out))))))
	 (module-undefined-list mod))
	(when missing
	  (##sys#error "module unresolved" name))
	(let* ((iexports 
		(map (lambda (exp)
		       (cond ((symbol? (cdr exp)) exp)
			     ((assq (car exp) (##sys#macro-environment)))
			     (else (##sys#error "(internal) indirect export not found" (car exp)))) )
		     (module-indirect-exports mod)))
	       (new-se (merge-se 
			(##sys#macro-environment) 
			(##sys#current-environment) 
			iexports vexports sexports sdlist)))
	  (##sys#mark-imported-symbols iexports)
	  (for-each
	   (lambda (m)
	     (let ((se (merge-se (cadr m) new-se))) ;XXX needed?
	       (dm `(FIXUP: ,(car m) ,@(map-se se)))
	       (set-car! (cdr m) se)))
	   sdlist)
	  (dm `(EXPORTS: 
		,(module-name mod) 
		(DLIST: ,@dlist)
		(SDLIST: ,@(map-se sdlist))
		(IEXPORTS: ,@(map-se iexports))
		(VEXPORTS: ,@(map-se vexports))
		(SEXPORTS: ,@(map-se sexports))))
	  (set-module-vexports! mod vexports)
	  (set-module-sexports! mod sexports)
	  (set-module-iexports! 
	   mod
	   (merge-se (module-iexports mod) iexports)) ; "reexport" may already have added some
	  (set-module-saved-environments!
	   mod
	   (cons (merge-se (##sys#current-environment) vexports sexports)
		 (##sys#macro-environment))))))))

(define ##sys#module-table '())


;;; Import-expansion

(define (##sys#find-module/import-library mname loc)
  (let* ((mname (##sys#resolve-module-name mname loc))
	 (mod (##sys#find-module mname #f loc)))
    (unless mod
      (let* ((il (##sys#find-extension
		  (string-append (symbol->string mname) ".import")
		  #t)))
	(cond (il (parameterize ((##sys#current-module #f)
				 (##sys#current-environment '())
				 (##sys#current-meta-environment 
				  (##sys#current-meta-environment))
				 (##sys#macro-environment
				  (##sys#meta-macro-environment)))
		    (fluid-let ((##sys#notices-enabled #f)) ; to avoid re-import warnings
		      (##sys#load il #f #f)))
		  (set! mod (##sys#find-module mname 'import)))
	      (else
	       (##sys#syntax-error-hook
		loc "cannot import from undefined module" 
		mname)))))
    mod))

(define (##sys#expand-import x r c import-env macro-env meta? reexp? loc)
  (let ((%only (r 'only))
	(%rename (r 'rename))
	(%except (r 'except))
	(%prefix (r 'prefix))
	(%srfi (r 'srfi)))
    (define (resolve sym)
      (or (lookup sym '()) sym))	;XXX really empty se?
    (define (tostr x)
      (cond ((string? x) x)
	    ((keyword? x) (##sys#string-append (##sys#symbol->string x) ":")) ; hack
	    ((symbol? x) (##sys#symbol->string x))
	    ((number? x) (number->string x))
	    (else (##sys#syntax-error-hook loc "invalid prefix" ))))
    (define (import-name spec)
      (let* ((mod (##sys#find-module/import-library (##sys#strip-syntax spec) 'import))
	     (vexp (module-vexports mod))
	     (sexp (module-sexports mod))
	     (iexp (module-iexports mod)))
	(values vexp sexp iexp)))
    (define (import-spec spec)
      (cond ((symbol? spec) (import-name spec))
	    ((or (not (list? spec)) (< (length spec) 2))
	     (##sys#syntax-error-hook loc "invalid import specification" spec))
	    ((and (c %srfi (car spec)) (fixnum? (cadr spec)) (null? (cddr spec))) ; only one number
	     (import-name 
	      (##sys#intern-symbol
	       (##sys#string-append "srfi-" (##sys#number->string (cadr spec))))))
	    (else
	     (let ((s (car spec)))
	       (let-values (((impv imps impi) (import-spec (cadr spec))))
		 (cond ((c %only s)
			(##sys#check-syntax loc spec '(_ _ . #(symbol 0)))
			(let ((ids (map resolve (cddr spec))))
			  (let loop ((ids ids) (v '()) (s '()))
			    (cond ((null? ids) (values v s impi))
				  ((assq (car ids) impv) =>
				   (lambda (a) 
				     (loop (cdr ids) (cons a v) s)))
				  ((assq (car ids) imps) =>
				   (lambda (a) 
				     (loop (cdr ids) v (cons a s))))
				  (else (loop (cdr ids) v s))))))
		       ((c %except s)
			(##sys#check-syntax loc spec '(_ _ . #(symbol 0)))
			(let ((ids (map resolve (cddr spec))))
			  (let loop ((impv impv) (v '()))
			    (cond ((null? impv)
				   (let loop ((imps imps) (s '()))
				     (cond ((null? imps) (values v s impi))
					   ((memq (caar imps) ids) (loop (cdr imps) s))
					   (else (loop (cdr imps) (cons (car imps) s))))))
				  ((memq (caar impv) ids) (loop (cdr impv) v))
				  (else (loop (cdr impv) (cons (car impv) v)))))))
		       ((c %rename s)
			(##sys#check-syntax loc spec '(_ _ . #((symbol symbol) 0)))
			(let loop ((impv impv) (imps imps) (v '()) (s '()) (ids (cddr spec)))
			  (cond ((null? impv) 
				 (cond ((null? imps)
					(for-each
					 (lambda (id)
					   (##sys#warn "renamed identifier not imported" id) )
					 ids)
					(values v s impi))
				       ((assq (caar imps) ids) =>
					(lambda (a)
					  (loop impv (cdr imps)
						v
						(cons (cons (cadr a) (cdar imps)) s)
						(##sys#delq a ids))))
				       (else (loop impv (cdr imps) v (cons (car imps) s) ids))))
				((assq (caar impv) ids) =>
				 (lambda (a)
				   (loop (cdr impv) imps
					 (cons (cons (cadr a) (cdar impv)) v)
					 s
					 (##sys#delq a ids))))
				(else (loop (cdr impv) imps
					    (cons (car impv) v)
					    s ids)))))
		       ((c %prefix s)
			(##sys#check-syntax loc spec '(_ _ _))
			(let ((pref (tostr (caddr spec))))
			  (define (ren imp)
			    (cons 
			     (##sys#string->symbol 
			      (##sys#string-append pref (##sys#symbol->string (car imp))) )
			     (cdr imp) ) )
			  (values (map ren impv) (map ren imps) impi)))
		       (else (##sys#syntax-error-hook loc "invalid import specification" spec))))))))
    (##sys#check-syntax loc x '(_ . #(_ 1)))
    (let ((cm (##sys#current-module)))
      (when cm
	;; save import form
	(if meta?
	    (set-module-meta-import-forms! 
	     cm
	     (append (module-meta-import-forms cm) (cdr x)))
	    (set-module-import-forms!
	     cm 
	     (append (module-import-forms cm) (cdr x)))))
      (for-each
       (lambda (spec)
	 (let-values (((vsv vss vsi) (import-spec spec)))
	   (let ((prims '()))
	     (dd `(IMPORT: ,loc))
	     (dd `(V: ,(if cm (module-name cm) '<toplevel>) ,(map-se vsv)))
	     (dd `(S: ,(if cm (module-name cm) '<toplevel>) ,(map-se vss)))
	     (##sys#mark-imported-symbols vsv) ; mark imports as ##core#aliased
	     (for-each
	      (lambda (imp)
		(let* ((id (car imp))
		       (aid (cdr imp))
		       (prim (getp aid '##core#primitive)))
		  (when prim
		    (set! prims (cons imp prims)))
		  (and-let* ((a (assq id (import-env)))
			     ((not (eq? aid (cdr a)))))
		    (##sys#notice "re-importing already imported identifier" id))))
	      vsv)
	     (for-each
	      (lambda (imp)
		(and-let* ((a (assq (car imp) (macro-env)))
			   ((not (eq? (cdr imp) (cdr a)))))
		  (##sys#notice "re-importing already imported syntax" (car imp))) )
	      vss)
	     (when reexp?
	       (unless cm
		 (##sys#syntax-error-hook loc "`reexport' only valid inside a module"))
	       (let ((el (module-export-list cm)))
		 (cond ((eq? #t el)
			(set-module-sexports! cm (append vss (module-sexports cm)))
			(set-module-exist-list!
			 cm
			 (append (module-exist-list cm)
				 (map car vsv)
				 (map car vss))))
		       (else
			(set-module-export-list!
			 cm
			 (append
			  (let ((xl (module-export-list cm)))
			    (if (eq? #t xl) '() xl))
			  (map car vsv)
			  (map car vss))))))
	       (set-module-iexports! 
		cm
		(merge-se (module-iexports cm) vsi))
	       (when (pair? prims)
		 (set-module-meta-expressions! 
		  cm
		  (append
		   (module-meta-expressions cm)
		   `((##sys#mark-primitive ',prims)))))
	       (dm "export-list: " (module-export-list cm)))
	     (import-env (append vsv (import-env)))
	     (macro-env (append vss (macro-env))))))
       (cdr x))
      '(##core#undefined))))

(define (##sys#module-rename sym prefix)
  (##sys#string->symbol
   (string-append
    (##sys#slot prefix 1)
    "#"
    (##sys#slot sym 1) ) ) )

(define (##sys#alias-global-hook sym assign where)
  (define (mrename sym)
    (cond ((##sys#current-module) => 
	   (lambda (mod)
	     (dm "(ALIAS) global alias " sym " in " (module-name mod))
	     (unless assign 
	       (##sys#register-undefined sym mod where))
	     (##sys#module-rename sym (module-name mod))))
	  (else sym)))
  (cond ((##sys#qualified-symbol? sym) sym)
	((getp sym '##core#primitive) =>
	 (lambda (p)
	   (dm "(ALIAS) primitive: " p)
	   p))
	((getp sym '##core#aliased) 
	 (dm "(ALIAS) marked: " sym)
	 sym)
	((assq sym ((##sys#active-eval-environment))) =>
	 (lambda (a)
	   (let ((sym2 (cdr a)))
	     (dm "(ALIAS) in current environment " sym " -> " sym2)
	     (if (pair? sym2)		; macro (XXX can this be?)
		 (mrename sym)
		 (or (getp sym2 '##core#primitive) sym2)))))
	(else (mrename sym))))

(define (##sys#register-interface name exps)
  ;; expects exps to be stripped and validated
  (putp name '##core#interface exps))

(define (##sys#validate-exports exps loc)
  ;; expects "exps" to be stripped
  (define (err . args)
    (apply ##sys#syntax-error-hook loc args))
  (define (iface name)
    (or (getp name '##core#interface)
	(err "unknown interface" x exps)))
  (cond ((eq? '* exps) exps)
	((symbol? exps) (iface exps))
	((not (list? exps))
	 (err "invalid exports" exps))
	(else
	 (let loop ((xps exps))
	   (cond ((null? xps) '())
		 ((not (pair? xps))
		  (err "invalid exports" exps))
		 (else
		  (let ((x (car xps)))
		    (cond ((symbol? x) (cons x (loop (cdr xps))))
			  ((not (list? x))
			   (err "invalid export" x exps))
			  ((eq? #:syntax (car x))
			   (cons (cdr x) (loop (cdr xps)))) ; currently not used
			  ((eq? #:interface (car x))
			   (if (and (pair? (cdr x)) (symbol? (cadr x)))
			       (iface (cadr x))
			       (err "invalid interface specification" x exps)))
			  (else
			   (let loop2 ((lst x))
			     (cond ((null? lst) (cons x (loop (cdr xps))))
				   ((symbol? (car lst)) (loop2 (cdr lst)))
				   (else (err "invalid export" x exps)))))))))))))

(define (##sys#register-functor name fargs fexps body)
  (putp name '##core#functor (cons fargs (cons fexps body))))

(define (##sys#instantiate-functor name fname args)
  (let ((funcdef (getp fname '##core#functor)))
    (define (err . args)
      (apply ##sys#syntax-error-hook name args))
    (unless funcdef (err "instantation of undefined functor" fname))
    (let ((fargs (car funcdef))
	  (exports (cadr funcdef))
	  (body (cddr funcdef)))
      (define (merr)
	(err "argument list mismatch in functor instantiation" 
	     (cons name args) (cons fname (map car fargs))))
      `(##core#let-module-alias
	,(let loop ((as args) (fas fargs))
	   (cond ((null? as) (if (null? fas) '() (merr)))
		 ((null? fas) (merr))
		 (else
		  (let* ((p (car fas))
			 (alias (car p))
			 (mname (car as))
			 (exps (cdr p)))
		    (##sys#match-functor-argument alias name mname exps fname)
		    (cons (list alias mname) (loop (cdr as) (cdr fas)))))))
	(##core#module
	 ,name
	 ,(if (eq? '* exports) #t exports)
	 ,@body)))))

(define (##sys#match-functor-argument alias name mname exps fname)
  (let ((mod (##sys#find-module (##sys#resolve-module-name mname 'module) #t 'module)))
    (unless (eq? exps '*)
      (let ((missing '()))
	(for-each
	 (lambda (exp)
	   (let ((sym (if (symbol? exp) exp (car exp))))
	     (unless (##sys#find-export sym mod #f)
	       (set! missing (cons sym missing)))))
	 exps)
	(when (pair? missing)
	  (##sys#syntax-error-hook
	   'module 
	   (apply
	    string-append 
	    "argument module `" (symbol->string mname) "' does not match required signature\n"
	    "in instantiation `" (symbol->string name) "' of functor `"
	    (symbol->string fname) "', because the following required exports are missing:\n"
	    (map (lambda (s) (string-append "\n  " (symbol->string s))) missing))))))))


;;; built-in modules (needed for eval environments)

(let ((r4rs-values
       '(not boolean? eq? eqv? equal? pair?
	     cons car cdr caar cadr cdar cddr caaar caadr cadar caddr cdaar cdadr
	     cddar cdddr caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr cdaaar
	     cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr set-car! set-cdr!
	     null? list? list length list-tail list-ref append reverse memq memv
	     member assq assv assoc symbol? symbol->string string->symbol number?
	     integer? exact? real? complex? inexact? rational? zero? odd? even?
	     positive? negative?  max min + - * / = > < >= <= quotient remainder
	     modulo gcd lcm abs floor ceiling truncate round exact->inexact
	     inexact->exact exp log expt sqrt sin cos tan asin acos atan
	     number->string string->number char? char=? char>? char<? char>=?
	     char<=? char-ci=? char-ci<? char-ci>?  char-ci>=? char-ci<=?
	     char-alphabetic? char-whitespace? char-numeric? char-upper-case?
	     char-lower-case? char-upcase char-downcase char->integer integer->char
	     string? string=?  string>? string<? string>=? string<=? string-ci=?
	     string-ci<? string-ci>? string-ci>=? string-ci<=?  make-string
	     string-length string-ref string-set! string-append string-copy
	     string->list list->string substring string-fill! vector? make-vector
	     vector-ref vector-set! string vector vector-length vector->list
	     list->vector vector-fill! procedure? map for-each apply force
	     call-with-current-continuation input-port? output-port?
	     current-input-port current-output-port call-with-input-file
	     call-with-output-file open-input-file open-output-file
	     close-input-port close-output-port load read eof-object? read-char
	     peek-char write display write-char newline with-input-from-file
	     with-output-to-file eval
	     char-ready? imag-part real-part magnitude numerator denominator
	     scheme-report-environment null-environment interaction-environment
	     else))
      (r4rs-syntax
       ;;XXX better would be to move these into the "chicken"
       ;;    module. "import[-for-syntax]" and "reexport" are in
       ;;    ##sys#initial-macro-environment and thus always available inside 
       ;;    modules.
       ##sys#default-macro-environment))
  (##sys#register-primitive-module 'r4rs r4rs-values r4rs-syntax)
  (##sys#register-primitive-module 
   'scheme
   (append '(dynamic-wind values call-with-values) r4rs-values)
   r4rs-syntax)
  (##sys#register-primitive-module 'r4rs-null '() r4rs-syntax)
  (##sys#register-primitive-module 'r5rs-null '() r4rs-syntax))

(##sys#register-module-alias 'r5rs 'scheme)

(register-feature! 'module-environments)

(define (module-environment mname #!optional (ename mname))
  (let* ((mod (##sys#find-module/import-library mname 'module-environment))
	 (saved (module-saved-environments mod)))
    (##sys#make-structure 'environment ename (car saved) #t)))
