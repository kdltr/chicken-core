;;;; batch-driver.scm - Driver procedure for the compiler
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
  (unit driver))

(include "compiler-namespace")
(include "tweaks")

(define-constant funny-message-timeout 60000)

(define user-options-pass (make-parameter #f))
(define user-read-pass (make-parameter #f))
(define user-preprocessor-pass (make-parameter #f))
(define user-pass (make-parameter #f))
(define user-post-analysis-pass (make-parameter #f))


;;; Compile a complete source file:

(define (compile-source-file filename . options)
  (define (option-arg p)
    (if (null? (cdr p))
	(quit "missing argument to `-~A' option" (car p))
	(let ([arg (cadr p)])
	  (if (symbol? arg)
	      (quit "invalid argument to `~A' option" arg)
	      arg) ) ) )
  (initialize-compiler)
  (set! explicit-use-flag (memq 'explicit-use options))
  (let ((initforms `((##core#declare
		      ,@(append 
			 default-declarations
			 (if explicit-use-flag
			     '()
			     `((uses ,@units-used-by-default)) ) ) ) ) )
        (verbose (memq 'verbose options))
	(outfile (cond ((memq 'output-file options) 
			=> (lambda (node)
			     (let ((oname (option-arg node)))
			       (if (symbol? oname)
				   (symbol->string oname)
				   oname) ) ) )
		       ((memq 'to-stdout options) #f)
		       (else (make-pathname #f (if filename (pathname-file filename) "out") "c")) ) )
	(ipath (map chop-separator
		    (string-split 
		     (or (get-environment-variable "CHICKEN_INCLUDE_PATH") "") 
		     ";")))
	(opasses default-optimization-passes)
	(time0 #f)
	(time-breakdown #f)
	(forms '())
	(inline-output-file #f)
	(type-output-file #f)
	(cleanup-forms '(((##sys#implicit-exit-handler))))
	(profile (or (memq 'profile options)
		     (memq 'accumulate-profile options) 
		     (memq 'profile-name options)))
	(profile-name 
	 (and-let* ((pn (memq 'profile-name options))) (cadr pn)))
	(hsize (memq 'heap-size options))
	(kwstyle (memq 'keyword-style options))
        (loop/dispatch (memq 'clustering options))
	(uses-units '())
	(uunit (memq 'unit options))
	(a-only (memq 'analyze-only options))
	(dynamic (memq 'dynamic options))
	(do-scrutinize (memq 'scrutinize options))
	(dumpnodes #f)
	(start-time #f)
	(upap #f)
	(wrap-module (memq 'module options))
	(ssize (or (memq 'nursery options) (memq 'stack-size options))) )

    (define (cputime) (current-milliseconds))

    (define (dribble fstr . args)
      (debugging 'p (apply sprintf fstr args)))

    (define (print-header mode dbgmode)
      (debugging 'p "pass" mode)
      (and (memq dbgmode debugging-chicken)
	   (begin
	     (printf "[~a]~%" mode)
	     #t) ) )

    (define (print-node mode dbgmode n)
      (when (print-header mode dbgmode)
	(if dumpnodes
	    (dump-nodes n)
	    (pretty-print (build-expression-tree n)) ) ) )

    (define (print-db mode dbgmode db pass)
      (when (print-header mode dbgmode)
	(printf "(iteration ~s)~%" pass)
	(display-analysis-database db) ) )

    (define (print-expr mode dbgmode xs)
      (when (print-header mode dbgmode)
	(for-each 
	 (lambda (x) 
	   (pretty-print x)
	   (newline))
	 xs) ) )

    (define (arg-val str)
      (let* ((len (string-length str))
	     (len1 (- len 1)) )
	(or (if (< len 2)
		(string->number str)
		(case (string-ref str len1)
		  ((#\m #\M) (* (string->number (substring str 0 len1)) (* 1024 1024)))
		  ((#\k #\K) (* (string->number (substring str 0 len1)) 1024))
		  (else (string->number str)) ) )
	    (quit "invalid numeric argument ~S" str) ) ) )

    (define (collect-options opt)
      (let loop ([opts options])
	(cond [(memq opt opts) => (lambda (p) (cons (option-arg p) (loop (cddr p))))]
	      [else '()] ) ) )

    (define (begin-time)
      (when time-breakdown (set! time0 (cputime))) )

    (define (end-time pass)
      (when time-breakdown
	(printf "milliseconds needed for ~a: \t~s~%" 
	  pass
	  (inexact->exact (round (- (cputime) time0)) ) )))

    (define (analyze pass node . args)
      (let-optionals args ((no 0) (contf #t))
        (let ((db (analyze-expression node)))
	  (when upap
	    (upap pass db node
		  (cut get db <> <>)
		  (cut put! db <> <> <>)
		  no contf) )
	  db) ) )

    (when uunit
      (set! unit-name (string->c-identifier (stringify (option-arg uunit)))) )
    (when (or unit-name dynamic)
      (set! standalone-executable #f))
    (when (memq 'ignore-repository options)
      (set! ##sys#dload-disabled #t)
      (repository-path #f))
    (set! enable-specialization (memq 'specialize options))
    (set! debugging-chicken 
      (append-map
       (lambda (do)
	 (map (lambda (c) (string->symbol (string c)))
	      (string->list do) ) )
       (collect-options 'debug) ) )
    (when (memq 'h debugging-chicken)
      (print-debug-options)
      (exit))
    (set! dumpnodes (memq '|D| debugging-chicken))
    (set! import-libraries
      (map (lambda (il)
	     (cons (string->symbol il) 
		   (string-append il ".import.scm")))
	   (collect-options 'emit-import-library)))
    (when (and (memq 'emit-all-import-libraries options)
	       (not a-only))
      (set! all-import-libraries #t))
    (set! enable-module-registration (not (memq 'no-module-registration options)))
    (when (or enable-specialization
	      (memq 'scrutinize options))
      (set! do-scrutinize #t))
    (when (memq 't debugging-chicken) (##sys#start-timer))
    (when (memq 'b debugging-chicken) (set! time-breakdown #t))
    (when (memq 'emit-exports options)	; OBSOLETE
      (warning "obsolete compiler option: -emit-exports") )
    (when (memq 'lambda-lift options)	; OBSOLETE
      (warning "obsolete compiler option: -lambda-lift") )
    (when (memq 'unboxing options)	; OBSOLETE
      (warning "obsolete compiler option: -unboxing") )
    (when (memq 'raw options)
      (set! explicit-use-flag #t)
      (set! cleanup-forms '())
      (set! initforms '()) )
    (when (memq 'no-lambda-info options)
      (set! emit-closure-info #f) )
    (when (memq 'no-compiler-syntax options)
      (set! compiler-syntax-enabled #f))
    (when (memq 'local options)
      (set! local-definitions #t))
    (when (memq 'inline-global options)
      (set! enable-inline-files #t)
      (set! inline-locally #t))
    (when verbose
      (set! ##sys#notices-enabled #t))
    (when (memq 'strict-types options)
      (set! strict-variable-types #t)
      (set! enable-specialization #t)
      (set! do-scrutinize #t))
    (when (memq 'no-warnings options) 
      (dribble "Warnings are disabled")
      (set! ##sys#warnings-enabled #f) 
      (set! do-scrutinize #f))		; saves some processing time
    (when (memq 'optimize-leaf-routines options) (set! optimize-leaf-routines #t))
    (when (memq 'unsafe options) 
      (set! unsafe #t) )
    (when (memq 'setup-mode options)
      (set! ##sys#setup-mode #t))
    (when (memq 'disable-interrupts options) (set! insert-timer-checks #f))
    (when (memq 'fixnum-arithmetic options) (set! number-type 'fixnum))
    (when (memq 'block options) (set! block-compilation #t))
    (when (memq 'emit-external-prototypes-first options)
      (set! external-protos-first #t))
    (when (memq 'inline options) (set! inline-locally #t))
    (and-let* ((ifile (memq 'emit-inline-file options)))
      (set! inline-locally #t)		; otherwise this option makes no sense
      (set! local-definitions #t)
      (set! inline-output-file (option-arg ifile)))
    (and-let* ((tfile (memq 'emit-type-file options)))
      (set! type-output-file (option-arg tfile)))
    (and-let* ([inlimit (memq 'inline-limit options)])
      (set! inline-max-size 
	(let ([arg (option-arg inlimit)])
	  (or (string->number arg)
	      (quit "invalid argument to `-inline-limit' option: `~A'" arg) ) ) ) )
    (when (memq 'case-insensitive options) 
      (dribble "Identifiers and symbols are case insensitive")
      (register-feature! 'case-insensitive)
      (case-sensitive #f) )
    (when kwstyle
      (let ([val (option-arg kwstyle)])
	(cond [(string=? "prefix" val) (keyword-style #:prefix)]
	      [(string=? "none" val) (keyword-style #:none)]
	      [(string=? "suffix" val) (keyword-style #:suffix)]
	      [else (quit "invalid argument to `-keyword-style' option")] ) ) )
    (when (memq 'no-parenthesis-synonyms options)
      (dribble "Disabled support for parenthesis synonyms")
      (parenthesis-synonyms #f) )
    (when (memq 'no-symbol-escape options) 
      (dribble "Disabled support for escaped symbols")
      (symbol-escape #f) )
    (when (memq '("-r5rs-syntax") options)
      (dribble "Disabled the Chicken extensions to R5RS syntax")
      (case-sensitive #f)
      (keyword-style #:none)
      (parentheses-synonyms #f)
      (symbol-escape #f) )
    (set! verbose-mode verbose)
    (set! ##sys#read-error-with-line-number #t)
    (set! ##sys#include-pathnames
      (append (map chop-separator (collect-options 'include-path))
	      ##sys#include-pathnames
	      ipath) )
    (when (and outfile filename (string=? outfile filename))
      (quit "source- and output-filename are the same") )
    (set! uses-units
      (append-map
       (lambda (u) (map string->symbol (string-split u ", ")))
       (collect-options 'uses)))
    (when (memq 'keep-shadowed-macros options)
      (set! undefine-shadowed-macros #f) )
    (when (memq 'no-argc-checks options)
      (set! no-argc-checks #t) )
    (when (memq 'no-bound-checks options)
      (set! no-bound-checks #t) )
    (when (memq 'no-procedure-checks options)
      (set! no-procedure-checks #t) )
    (when (memq 'no-procedure-checks-for-toplevel-bindings options)
      (set! no-global-procedure-checks #t) )
    (when (memq 'no-procedure-checks-for-usual-bindings options)
      (for-each 
       (lambda (v)
         (mark-variable v '##compiler#always-bound-to-procedure)
         (mark-variable v '##compiler#always-bound) )
       default-standard-bindings)
      (for-each 
       (lambda (v)
         (mark-variable v '##compiler#always-bound-to-procedure)
         (mark-variable v '##compiler#always-bound) )
       default-extended-bindings) )
    (when (memq 'p debugging-chicken) (load-verbose #t))

    ;; Handle feature options:
    (for-each 
     register-feature!
     (append-map (cut string-split <> ", ") (collect-options 'feature)))
    (for-each 
     unregister-feature!
     (append-map (cut string-split <> ",") (collect-options 'no-feature)))

    ;; Load extensions:
    (set! ##sys#features (cons #:compiler-extension ##sys#features))
    (let ([extends (collect-options 'extend)])
      (dribble "Loading compiler extensions...")
      (for-each
       (lambda (f) (load (##sys#resolve-include-filename f #f #t))) 
       extends) )
    (set! ##sys#features (delete #:compiler-extension ##sys#features eq?))
    (set! ##sys#features (cons '#:compiling ##sys#features))
    (set! upap (user-post-analysis-pass))

    ;; Insert postponed initforms:
    (set! initforms (append initforms postponed-initforms))

    (let ((se (map string->symbol (collect-options 'static-extension)))) ; DEPRECATED
      ;; Append required extensions to initforms:
      (set! initforms
	(append 
	 initforms 
	 (map (lambda (r) `(##core#require-extension (,r) #t)) 
	      (append se (map string->symbol (collect-options 'require-extension))))))

      ;; add static-extensions as used units:
      (set! ##sys#explicit-library-modules
	(append ##sys#explicit-library-modules se)))

    (when (memq 'compile-syntax options)
      (set! ##sys#enable-runtime-macros #t) )
    (set! target-heap-size
      (and hsize
	   (arg-val (option-arg hsize))))
    (set! target-stack-size
      (and ssize
	   (arg-val (option-arg ssize))))
    (set! emit-trace-info (not (memq 'no-trace options)))
    (set! disable-stack-overflow-checking (memq 'disable-stack-overflow-checks options))
    (set! bootstrap-mode (feature? #:chicken-bootstrap))
    (when (memq 'm debugging-chicken) (set-gc-report! #t))
    (unless (memq 'no-usual-integrations options)
      (set! standard-bindings default-standard-bindings)
      (set! extended-bindings default-extended-bindings) )
    (dribble "debugging info: ~A"
	     (if emit-trace-info
		 "calltrace"
		 "none") )
    (when profile
      (let ((acc (eq? 'accumulate-profile (car profile))))
	(when (and acc (not profile-name))
	  (quit
	   "you need to specify -profile-name if using accumulated profiling runs"))
	(set! emit-profile #t)
	(set! profiled-procedures 'all)
	(set! initforms
	  (append
	   initforms
	   default-profiling-declarations
	   (if acc
	       '((set! ##sys#profile-append-mode #t))
	       '() ) ) )
	(dribble "generating ~aprofiled code" (if acc "accumulative " "")) ))

    ;;XXX hardcoded "modules.db" is bad (also used in chicken-install.scm)
    (load-identifier-database "modules.db")

    (cond ((memq 'version options)
	   (print-version #t)
	   (newline) )
	  ((or (memq 'help options) (memq '-help options) (memq 'h options) (memq '-h options))
	   (print-usage))
	  ((memq 'release options)
	   (display (chicken-version)) 
	   (newline) )
	  ((not filename)
	   (print-version #t)
	   (display "\nEnter `chicken -help' for information on how to use the compiler,\n")
	   (display "or try `csc' for a more convenient interface.\n")
	   (display "\nRun `csi' to start the interactive interpreter.\n"))
	  (else

	   ;; Display header:
	   (dribble "compiling `~a' ..." filename)
	   (set! source-filename filename)
	   (debugging 'r "options" options)
	   (debugging 'r "debugging options" debugging-chicken)
	   (debugging 'r "target heap size" target-heap-size)
	   (debugging 'r "target stack size" target-stack-size)
	   (set! start-time (cputime))

	   ;; Read toplevel expressions:
	   (set! ##sys#line-number-database (make-vector line-number-database-size '()))
	   (let ([prelude (collect-options 'prelude)]
		 [postlude (collect-options 'postlude)] 
		 [files (append 
			 (collect-options 'prologue)
			 (list filename)
			 (collect-options 'epilogue) ) ]  )

	     (let ([proc (user-read-pass)])
	       (cond [proc
		      (dribble "User read pass...")
		      (set! forms (proc prelude files postlude)) ]
		     [else
		      (do ([files files (cdr files)])
			  ((null? files)
			   (set! forms
			     (append (map string->expr prelude)
				     (reverse forms)
				     (map string->expr postlude) ) ) )
			(let* ((f (car files))
			       (in (check-and-open-input-file f)) )
			  (fluid-let ((##sys#current-source-filename f))
			    (let loop ()
			      (let ((x (read/source-info in)))
				(cond ((eof-object? x) 
				       (close-checked-input-file in f) )
				      (else
				       (set! forms (cons x forms))
				       (loop)))))))) ] ) ) )

	   ;; Start compilation passes:
	   (let ([proc (user-preprocessor-pass)])
	     (when proc
	       (dribble "User preprocessing pass...")
	       (set! forms (map proc forms))))

	   (print-expr "source" '|1| forms)
	   (begin-time)
	   (unless (null? uses-units)
	     (set! ##sys#explicit-library-modules
	       (append ##sys#explicit-library-modules uses-units))
	     (set! forms (cons `(declare (uses ,@uses-units)) forms)) )
	   (let* ((exps0 (map canonicalize-expression
			      (let ((forms (append initforms forms)))
				(if wrap-module
				    `((##core#module main () 
						     (import scheme chicken)
						     ,@forms))
				    forms))))
		  [pvec (gensym)]
		  [plen (length profile-lambda-list)]
		  [exps (append
			 (map (lambda (ic) `(set! ,(cdr ic) ',(car ic))) immutable-constants)
			 (map (lambda (n) `(##core#callunit ,n)) used-units)
			 (if emit-profile
			     `((set! ,profile-info-vector-name 
				 (##sys#register-profile-info
				  ',plen
				  ',(and (not unit-name)
					 (or profile-name #t)))))
			     '() )
			 (map (lambda (pl)
				`(##sys#set-profile-info-vector!
				  ,profile-info-vector-name
				  ',(car pl)
				  ',(cdr pl) ) )
			      profile-lambda-list)
			 exps0
			 (if (and (not unit-name) (not dynamic))
			     cleanup-forms
			     '() )
			 '((##core#undefined))) ] )

	     (when (pair? compiler-syntax-statistics)
	       (with-debugging-output
		'S
		(lambda ()
		  (print "applied compiler syntax:")
		  (for-each 
		   (lambda (cs) (printf "  ~a\t\t~a~%" (car cs) (cdr cs)))
		   compiler-syntax-statistics))))
   	     (when (debugging '|N| "real name table:")
	       (display-real-name-table) )
	     (when (debugging 'n "line number database:")
	       (display-line-number-database) )

	     (when (and unit-name dynamic)
	       (##sys#notice 
		(sprintf "library unit `~a' compiled in dynamic mode" unit-name) ) )

	     (set! ##sys#line-number-database line-number-database-2)
	     (set! line-number-database-2 #f)

	     (end-time "canonicalization")
	     (print-expr "canonicalized" '|2| exps)

	     (when (memq 'check-syntax options) (exit))

	     (let ([proc (user-pass)])
	       (when proc
		 (dribble "User pass...")
		 (begin-time)
		 (set! exps (map proc exps))
		 (end-time "user pass") ) )

	     (let ((node0 (make-node
			   'lambda '(())
			   (list (build-node-graph
				  (canonicalize-begin-body exps) ) ) ) ) 
		   (db #f))
	       (print-node "initial node tree" '|T| node0)
	       (initialize-analysis-database)

	       ;; collect requirements and load inline files
	       (let* ((req (concatenate (vector->list file-requirements)))
		      (mreq (concatenate (map cdr req))))
		 (when (debugging 'M "; requirements:")
		   (pp req))
		 (when enable-inline-files
		   (for-each
		    (lambda (id)
		      (and-let* ((ifile (##sys#resolve-include-filename 
					 (make-pathname #f (symbol->string id) "inline")
					 #f #t))
				 ((file-exists? ifile)))
			(dribble "Loading inline file ~a ..." ifile)
			(load-inline-file ifile)))
		    mreq))
		 (let ((ifs (collect-options 'consult-inline-file)))
		   (unless (null? ifs)
		     (set! inline-locally #t)
		     (for-each 
		      (lambda (ilf)
			(dribble "Loading inline file ~a ..." ilf)
			(load-inline-file ilf) )
		      ifs)))
		 (when (or do-scrutinize enable-specialization)
		   ;;XXX hardcoded database file name
		   (unless (memq 'ignore-repository options)
		     (unless (load-type-database "types.db")
		       (quit "default type-database `types.db' not found")))
		   (for-each 
		    (lambda (fn)
		      (or (load-type-database fn #f)
			  (quit "type-database `~a' not found" fn)))
		    (collect-options 'types))
		   (for-each
		    (lambda (id)
		      (load-type-database (make-pathname #f (symbol->string id) "types")))
		    mreq)
		   (begin-time)
		   (set! first-analysis #f)
		   (set! db (analyze 'scrutiny node0))
		   (print-db "analysis" '|0| db 0)
		   (end-time "pre-analysis (scrutiny)")
		   (begin-time)
		   (debugging 'p "performing scrutiny")
		   (scrutinize node0 db do-scrutinize enable-specialization)
		   (end-time "scrutiny")
		   (when enable-specialization
		     (print-node "specialization" '|P| node0))
		   (set! first-analysis #t) ) )

	       (set! ##sys#line-number-database #f)
	       (set! constant-table #f)
	       (set! inline-table #f)
	       (unless unsafe
		 (scan-toplevel-assignments (first (node-subexpressions node0))) )

	       (begin-time)
	       (let ([node1 (perform-cps-conversion node0)])
		 (end-time "cps conversion")
		 (print-node "cps" '|3| node1)

		 ;; Optimization loop:
		 (let loop ((i 1)
			    (node2 node1)
			    (progress #t)
			    (l/d #f)
			    (l/d-done #f))
		   (begin-time)
		   (let ([db (analyze 'opt node2 i progress)])
		     (when first-analysis
		       (when (memq 'u debugging-chicken)
			 (dump-undefined-globals db))
		       (when (memq 'd debugging-chicken)
			 (dump-defined-globals db))
		       (when (memq 'v debugging-chicken)
			 (dump-global-refs db))
		       ;; do this here, because we must make sure we have a db
		       (when type-output-file
			 (dribble "generating type file `~a' ..." type-output-file)
			 (emit-type-file type-output-file db)))
		     (set! first-analysis #f)
		     (end-time "analysis")
		     (print-db "analysis" '|4| db i)

		     (when (memq 's debugging-chicken) 
		       (print-program-statistics db))

		     (cond (progress
			    (debugging 'p "optimization pass" i)
			    (begin-time)
			    (receive (node2 progress-flag)
				(if l/d
				    (determine-loop-and-dispatch node2 db)
				    (perform-high-level-optimizations node2 db))
			      (end-time "optimization")
			      (print-node "optimized-iteration" '|5| node2)
			      (cond (progress-flag
				     (loop (add1 i) node2 #t #f l/d))
				    ((and (not l/d-done) loop/dispatch)
				     (debugging 'p "clustering enabled")
				     (loop (add1 i) node2 #t #t #t))
				    ((not inline-substitutions-enabled)
				     (debugging 'p "rewritings enabled")
				     (set! inline-substitutions-enabled #t)
				     (loop (add1 i) node2 #t #f l/d-done) )
				    (optimize-leaf-routines
				     (begin-time)
				     (let ([db (analyze 'leaf node2)])
				       (end-time "analysis")
				       (begin-time)
				       (let ((progress
					      (transform-direct-lambdas! node2 db)))
					 (end-time "leaf routine optimization")
					 (loop (add1 i) 
					       node2
					       progress
					       #f
					       l/d-done) ) ) )
				    (else
				     (loop (add1 i) node2 #f #f l/d-done)) ) ) )
			   
			   (else
			    (print-node "optimized" '|7| node2)
			    ;; inlining into a file with interrupts enabled would
			    ;; change semantics
			    (when (and inline-output-file insert-timer-checks)
			      (let ((f inline-output-file))
				(dribble "generating global inline file `~a' ..." f)
				(emit-global-inline-file f db) ) )
			    (begin-time)
			    (set! node2 (perform-closure-conversion node2 db))
			    (end-time "closure conversion")
			    (print-db "final-analysis" '|8| db i)
			    (when (and ##sys#warnings-enabled
				       (> (- (cputime) start-time) funny-message-timeout))
			      (display "(don't worry - still compiling...)\n") )
			    (print-node "closure-converted" '|9| node2)
			    (when a-only (exit 0))
			    (begin-time)
			    (receive 
			     (node literals lliterals lambda-table)
			     (prepare-for-code-generation node2 db)
			     (end-time "preparation")
			     (begin-time)
			     (let ((out (if outfile (open-output-file outfile) (current-output-port))) )
			       (dribble "generating `~A' ..." outfile)
			       (generate-code literals lliterals lambda-table out filename dynamic db)
			       (when outfile
				 (close-output-port out)))
			     (end-time "code generation")
			     (when (memq 't debugging-chicken)
			       (##sys#display-times (##sys#stop-timer)))
			     (compiler-cleanup-hook)
			     (dribble "compilation finished.") ) ) ) ) ) ) ) ) ) ) ) )
