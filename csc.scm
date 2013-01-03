;;;; csc.scm - Driver program for the CHICKEN compiler - felix -*- Scheme -*-
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
  (block)
  (uses data-structures ports srfi-1 srfi-13 utils files extras))

(define-foreign-variable INSTALL_BIN_HOME c-string "C_INSTALL_BIN_HOME")
(define-foreign-variable INSTALL_CC c-string "C_INSTALL_CC")
(define-foreign-variable INSTALL_CXX c-string "C_INSTALL_CXX")
(define-foreign-variable INSTALL_RC_COMPILER c-string "C_INSTALL_RC_COMPILER")
(define-foreign-variable TARGET_CC c-string "C_TARGET_CC")
(define-foreign-variable TARGET_CXX c-string "C_TARGET_CXX")
(define-foreign-variable TARGET_RC_COMPILER c-string "C_TARGET_RC_COMPILER")
(define-foreign-variable TARGET_CFLAGS c-string "C_TARGET_CFLAGS")
(define-foreign-variable INSTALL_CFLAGS c-string "C_INSTALL_CFLAGS")
(define-foreign-variable TARGET_LDFLAGS c-string "C_TARGET_LDFLAGS")
(define-foreign-variable TARGET_FEATURES c-string "C_TARGET_FEATURES")
(define-foreign-variable INSTALL_LDFLAGS c-string "C_INSTALL_LDFLAGS")
(define-foreign-variable INSTALL_MORE_LIBS c-string "C_INSTALL_MORE_LIBS")
(define-foreign-variable INSTALL_MORE_STATIC_LIBS c-string "C_INSTALL_MORE_STATIC_LIBS")
(define-foreign-variable INSTALL_SHARE_HOME c-string "C_INSTALL_SHARE_HOME")
(define-foreign-variable INSTALL_LIB_HOME c-string "C_INSTALL_LIB_HOME")
(define-foreign-variable INSTALL_LIB_NAME c-string "C_INSTALL_LIB_NAME")
(define-foreign-variable INSTALL_INCLUDE_HOME c-string "C_INSTALL_INCLUDE_HOME")
(define-foreign-variable INSTALL_STATIC_LIB_HOME c-string "C_INSTALL_STATIC_LIB_HOME")
(define-foreign-variable TARGET_MORE_LIBS c-string "C_TARGET_MORE_LIBS")
(define-foreign-variable TARGET_MORE_STATIC_LIBS c-string "C_TARGET_MORE_STATIC_LIBS")
(define-foreign-variable TARGET_BIN_HOME c-string "C_TARGET_BIN_HOME")
(define-foreign-variable TARGET_SHARE_HOME c-string "C_TARGET_SHARE_HOME")
(define-foreign-variable TARGET_LIB_HOME c-string "C_TARGET_LIB_HOME")
(define-foreign-variable TARGET_LIB_NAME c-string "C_TARGET_LIB_NAME")
(define-foreign-variable TARGET_INCLUDE_HOME c-string "C_TARGET_INCLUDE_HOME")
(define-foreign-variable TARGET_STATIC_LIB_HOME c-string "C_TARGET_STATIC_LIB_HOME")
(define-foreign-variable TARGET_RUN_LIB_HOME c-string "C_TARGET_RUN_LIB_HOME")
(define-foreign-variable CHICKEN_PROGRAM c-string "C_CHICKEN_PROGRAM")
(define-foreign-variable CSC_PROGRAM c-string "C_CSC_PROGRAM")
(define-foreign-variable WINDOWS_SHELL bool "C_WINDOWS_SHELL")
(define-foreign-variable BINARY_VERSION int "C_BINARY_VERSION")


;;; Parameters:

(define mingw (eq? (build-platform) 'mingw32))
(define osx (eq? (software-version) 'macosx))
(define win mingw)
(define netbsd (eq? (software-version) 'netbsd))
(define cygwin (eq? (build-platform) 'cygwin))

(define elf
  (memq (software-version) '(linux netbsd freebsd solaris openbsd)))

(define (quit msg . args)
  (fprintf (current-error-port) "~a: ~?~%" CSC_PROGRAM msg args)
  (exit 64) )

(define chicken-prefix (get-environment-variable "CHICKEN_PREFIX"))
(define arguments (command-line-arguments))
(define host-mode (member "-host" arguments))
(define cross-chicken (##sys#fudge 39))

(define (prefix str dir default)
  (if chicken-prefix
      (make-pathname (list chicken-prefix dir) str)
      default) )

(define (quotewrap str)
  (qs (normalize-pathname str)))

(define home
  (quotewrap 
   (prefix "" "share" (if host-mode INSTALL_SHARE_HOME TARGET_SHARE_HOME))))

(define translator
  (quotewrap 
   (prefix "chicken" "bin"
	   (make-pathname
	    INSTALL_BIN_HOME
	    CHICKEN_PROGRAM))))

(define compiler (quotewrap (if host-mode INSTALL_CC TARGET_CC)))
(define c++-compiler (quotewrap (if host-mode INSTALL_CXX TARGET_CXX)))
(define rc-compiler (quotewrap (if host-mode INSTALL_RC_COMPILER TARGET_RC_COMPILER)))
(define linker (quotewrap (if host-mode INSTALL_CC TARGET_CC)))
(define c++-linker (quotewrap (if host-mode INSTALL_CXX TARGET_CXX)))
(define object-extension "o")
(define library-extension "a")
(define link-output-flag "-o ")
(define executable-extension "")
(define compile-output-flag "-o ")
(define nonstatic-compilation-options '())
(define shared-library-extension ##sys#load-dynamic-extension)
(define default-translation-optimization-options '())
(define pic-options (if (or mingw cygwin) '("-DPIC") '("-fPIC" "-DPIC")))
(define windows-shell WINDOWS_SHELL)
(define generate-manifest #f)

(define libchicken
  (if cygwin
      (string-append "cyg" INSTALL_LIB_NAME "-0")
      (string-append "lib" INSTALL_LIB_NAME)))

(define default-library
  (string-append libchicken "." library-extension))

(define default-compilation-optimization-options (string-split (if host-mode INSTALL_CFLAGS TARGET_CFLAGS)))
(define best-compilation-optimization-options default-compilation-optimization-options)
(define default-linking-optimization-options (string-split (if host-mode INSTALL_LDFLAGS TARGET_LDFLAGS)))
(define best-linking-optimization-options default-linking-optimization-options)
(define extra-features (if host-mode '() (string-split TARGET_FEATURES)))

(define-constant simple-options
  '(-explicit-use -no-trace -no-warnings -no-usual-integrations -optimize-leaf-routines -unsafe
    -block -disable-interrupts -fixnum-arithmetic -to-stdout -profile -raw -accumulate-profile
    -check-syntax -case-insensitive -shared -compile-syntax -no-lambda-info
    -dynamic -disable-stack-overflow-checks -local
    -emit-external-prototypes-first -inline -release -scrutinize
    -analyze-only -keep-shadowed-macros -inline-global -ignore-repository
    -no-symbol-escape -no-parentheses-synonyms -r5rs-syntax
    -no-argc-checks -no-bound-checks -no-procedure-checks -no-compiler-syntax
    -emit-all-import-libraries -setup-mode -no-elevation -no-module-registration
    -no-procedure-checks-for-usual-bindings -module
    -specialize -strict-types -clustering
    -lambda-lift -unboxing		; OBSOLETE
    -no-procedure-checks-for-toplevel-bindings))

(define-constant complex-options
  '(-debug -heap-size -nursery -stack-size -compiler -unit -uses -keyword-style
    -optimize-level -include-path -database-size -extend -prelude -postlude -prologue -epilogue 
    -inline-limit -profile-name
    -emit-inline-file -types -emit-type-file
    -feature -debug-level 
    -heap-growth -heap-shrinkage -heap-initial-size ; DEPRECATED
    -consult-inline-file
    -emit-import-library
    -no-feature))

(define-constant shortcuts
  '((-h "-help")
    (-s "-shared")
    (-S "-scrutinize")
    (-M "-module")
    (|-P| "-check-syntax")
    (|-V| "-version")			; DEPRECATED
    (-f "-fixnum-arithmetic")
    (|-D| "-feature")
    (-i "-case-insensitive")
    (|-K| "-keyword-style")
    (|-X| "-extend")
    (|-J| "-emit-all-import-libraries")
    (-x "-explicit-use")
    (-u "-unsafe")
    (-j "-emit-import-library")
    (-n "-emit-inline-file")
    (-b "-block") ) )

(define short-options
  (string->list "PHhsfiENxubvwAOeWkctgSJM") )


;;; Variables:

(define scheme-files '())
(define generated-scheme-files '())
(define c-files '())
(define rc-files '())
(define generated-c-files '())
(define generated-rc-files '())
(define object-files '())
(define generated-object-files '())
(define cpp-mode #f)
(define objc-mode #f)
(define embedded #f)
(define inquiry-only #f)
(define show-cflags #f)
(define show-ldflags #f)
(define show-libs #f)
(define dry-run #f)
(define gui #f)
(define deploy #f)
(define deployed #f)
(define rpath #f)

(define extra-libraries
  (if host-mode
      INSTALL_MORE_STATIC_LIBS
      TARGET_MORE_STATIC_LIBS))

(define extra-shared-libraries 
  (if host-mode 
      INSTALL_MORE_LIBS
      TARGET_MORE_LIBS))

(define default-library-files 
  (list
   (quotewrap
    (prefix default-library "lib"
	    (string-append
	     (if host-mode INSTALL_LIB_HOME TARGET_LIB_HOME)
	     (string-append "/" default-library)))) ))

(define default-shared-library-files 
  (list (string-append "-l" (if host-mode INSTALL_LIB_NAME TARGET_LIB_NAME))))

(define library-files default-library-files)
(define shared-library-files default-shared-library-files)

(define translate-options '())

(define include-dir
  (let ((id (prefix ""
                    (make-pathname "include" "chicken")
		    (if host-mode INSTALL_INCLUDE_HOME TARGET_INCLUDE_HOME))))
    (and (not (member id '("/usr/include" "")))
	 id) ) )

(define compile-options '())

(define builtin-compile-options
  (append
   (if include-dir (list (conc "-I\"" include-dir "\"")) '())
   (cond ((get-environment-variable "CHICKEN_C_INCLUDE_PATH") => 
	  (lambda (path) 
	    (map (cut string-append "-I\"" <> "\"") (string-split path ":;"))))
	 (else '()))))

(define compile-only-flag "-c")
(define translation-optimization-options default-translation-optimization-options)
(define compilation-optimization-options default-compilation-optimization-options)
(define linking-optimization-options default-linking-optimization-options)

(define library-dir
  (prefix "" "lib"
         (if host-mode
             INSTALL_LIB_HOME
             TARGET_LIB_HOME)) )

(define link-options '())

(define (builtin-link-options)
  (append
   (cond (elf
	  (list
	   (conc "-L\"" library-dir "\"")
	   (conc " -Wl,-R\""
		 (if (and deployed (not netbsd))
		     "\\$ORIGIN"
		     (prefix "" "lib"
			     (if host-mode
				 INSTALL_LIB_HOME
				 TARGET_RUN_LIB_HOME)))
		 "\"")) )
	 (else
	  (list (conc "-L\"" library-dir "\""))))
   (if (and deployed (eq? (software-version) 'freebsd))
       (list "-z origin")
       '())
   (cond ((get-environment-variable "CHICKEN_C_LIBRARY_PATH") => 
	  (lambda (path) 
	    (map (cut string-append "-L\"" <> "\"") (string-split path ":;"))))
	 (else '()))))
	
(define target-filename #f)
(define verbose #f)
(define keep-files #f)
(define translate-only #f)
(define compile-only #f)
(define to-stdout #f)
(define shared #f)
(define static #f)
(define static-libs #f)
(define required-extensions '())


;;; Display usage information:

(define (usage)
  (let ((csc CSC_PROGRAM))
    (print #<#EOF
Usage: #{csc} FILENAME | OPTION ...

  `#{csc}' is a driver program for the CHICKEN compiler. Files given on the
  command line are translated, compiled or linked as needed.

  FILENAME is a Scheme source file name with optional extension or a
  C/C++/Objective-C source, object or library file name with extension. OPTION
  may be one of the following:

  General options:

    -h  -help                      display this text and exit
    -v  -verbose                   show compiler notes and tool-invocations
    -vv                            display information about translation
                                    progress
    -vvv                           display information about all compilation
                                    stages
    -version                       display Scheme compiler version and exit
    -release                       display release number and exit

  File and pathname options:

    -o -output-file FILENAME       specifies target executable name
    -I -include-path PATHNAME      specifies alternative path for included
                                    files
    -to-stdout                     write compiler to stdout (implies -t)
    -s -shared -dynamic            generate dynamically loadable shared object
                                    file

  Language options:

    -D  -DSYMBOL  -feature SYMBOL  register feature identifier
    -no-feature SYMBOL             disable builtin feature identifier
    -c++                           compile via a C++ source file (.cpp) 
    -objc                          compile via Objective-C source file (.m)

  Syntax related options:

    -i -case-insensitive           don't preserve case of read symbols    
    -K -keyword-style STYLE        enable alternative keyword-syntax
                                    (prefix, suffix or none)
       -no-parentheses-synonyms    disables list delimiter synonyms
       -no-symbol-escape           disables support for escaped symbols
       -r5rs-syntax                disables the Chicken extensions to
                                    R5RS syntax
    -compile-syntax                macros are made available at run-time
    -j -emit-import-library MODULE write compile-time module information into
                                    separate file
    -J -emit-all-import-libraries  emit import-libraries for all defined modules
    -no-module-registration        do not generate module registration code
    -no-compiler-syntax            disable expansion of compiler-macros
    -M -module                     wrap compiled code into implicit module

  Translation options:

    -x  -explicit-use              do not use units `library' and `eval' by
                                    default
    -P  -check-syntax              stop compilation after macro-expansion
    -A  -analyze-only              stop compilation after first analysis pass

  Debugging options:

    -w  -no-warnings               disable warnings
    -d0 -d1 -d2 -debug-level NUMBER
                                   set level of available debugging information
    -no-trace                      disable rudimentary debugging information
    -profile                       executable emits profiling information 
    -accumulate-profile            executable emits profiling information in
                                    append mode
    -profile-name FILENAME         name of the generated profile information
                                    file
    -S  -scrutinize                perform local flow analysis
    -types FILENAME                load additional type database

  Optimization options:

    -O -O0 -O1 -O2 -O3 -O4 -O5 -optimize-level NUMBER
                                   enable certain sets of optimization options
    -optimize-leaf-routines        enable leaf routine optimization
    -no-usual-integrations         standard procedures may be redefined
    -u  -unsafe                    disable safety checks
    -local                         assume globals are only modified in current
                                    file
    -b  -block                     enable block-compilation
    -disable-interrupts            disable interrupts in compiled code
    -f  -fixnum-arithmetic         assume all numbers are fixnums
    -disable-stack-overflow-checks disables detection of stack-overflows
    -inline                        enable inlining
    -inline-limit LIMIT            set inlining threshold
    -inline-global                 enable cross-module inlining
    -specialize                    perform type-based specialization of primitive calls
    -n -emit-inline-file FILENAME  generate file with globally inlinable
                                    procedures (implies -inline -local)
    -consult-inline-file FILENAME  explicitly load inline file
    -emit-type-file FILENAME       write type-declaration information into file
    -no-argc-checks                disable argument count checks
    -no-bound-checks               disable bound variable checks
    -no-procedure-checks           disable procedure call checks
    -no-procedure-checks-for-usual-bindings
                                   disable procedure call checks only for usual
                                    bindings
    -no-procedure-checks-for-toplevel-bindings
                                   disable procedure call checks for toplevel
                                    bindings
    -strict-types                  assume variable do not change their type
    -clustering                    combine groups of local procedures into dispatch
                                     loop

  Configuration options:

    -unit NAME                     compile file as a library unit
    -uses NAME                     declare library unit as used.
    -heap-size NUMBER              specifies heap-size of compiled executable
    -nursery NUMBER  -stack-size NUMBER
                                   specifies nursery size of compiled
                                   executable
    -X -extend FILENAME            load file before compilation commences
    -prelude EXPRESSION            add expression to beginning of source file
    -postlude EXPRESSION           add expression to end of source file
    -prologue FILENAME             include file before main source file
    -epilogue FILENAME             include file after main source file

    -e  -embedded                  compile as embedded
                                    (don't generate `main()')
    -gui                           compile as GUI application
    -R  -require-extension NAME    require extension and import in compiled
                                    code
    -dll -library                  compile multiple units into a dynamic
                                    library
    -deploy                        deploy self-contained application bundle

  Options to other passes:

    -C OPTION                      pass option to C compiler
    -L OPTION                      pass option to linker
    -I<DIR>                        pass \"-I<DIR>\" to C compiler
                                    (add include path)
    -L<DIR>                        pass \"-L<DIR>\" to linker
                                    (add library path)
    -k                             keep intermediate files
    -c                             stop after compilation to object files
    -t                             stop after translation to C
    -cc COMPILER                   select other C compiler than the default
    -cxx COMPILER                  select other C++ compiler than the default
    -ld COMPILER                   select other linker than the default 
    -lLIBNAME                      link with given library
                                    (`libLIBNAME' on UNIX,
                                     `LIBNAME.lib' on Windows)
    -static-libs                   link with static CHICKEN libraries
    -static                        generate completely statically linked
                                    executable
    -F<DIR>                        pass \"-F<DIR>\" to C compiler
                                    (add framework header path on Mac OS X)
    -framework NAME                passed to linker on Mac OS X
    -rpath PATHNAME                add directory to runtime library search path
    -Wl,...                        pass linker options
    -strip                         strip resulting binary

  Inquiry options:

    -home                          show home-directory (where support files go)
    -cflags                        show required C-compiler flags and exit
    -ldflags                       show required linker flags and exit
    -libs                          show required libraries and exit
    -cc-name                       show name of default C compiler used
    -cxx-name                      show name of default C++ compiler used
    -ld-name                       show name of default linker used
    -dry-run                       just show commands executed, don't run them
                                    (implies `-v')

  Obscure options:

    -debug MODES                   display debugging output for the given modes
    -compiler PATHNAME             use other compiler than default `chicken'
    -raw                           do not generate implicit init- and exit code
    -emit-external-prototypes-first
                                   emit prototypes for callbacks before foreign
                                    declarations
    -ignore-repository             do not refer to repository for extensions
    -keep-shadowed-macros          do not remove shadowed macro
    -host                          compile for host when configured for
                                    cross-compiling
    -private-repository            load extensions from executable path
    -deployed                      link support file to be used from a deployed 
                                    executable (sets `rpath' accordingly, if supported
                                    on this platform)
    -no-elevation                  embed manifest on Windows to supress elevation
                                    warnings for programs named `install' or `setup'

  Options can be collapsed if unambiguous, so

    -vkfO

  is the same as

    -v -k -fixnum-arithmetic -optimize

  The contents of the environment variable CSC_OPTIONS are implicitly passed to
  every invocation of `#{csc}'.

EOF
;|        (for emacs font-lock)
  ) ) )


;;; Parse arguments:

(define (run args)

  (define (t-options . os)
    (set! translate-options (append translate-options os)) )

  (define (check o r . n)
    (unless (>= (length r) (optional n 1))
      (quit "not enough arguments to option `~A'" o) ) )

  (define (shared-build lib)
    (set! translate-options (cons* "-feature" "chicken-compile-shared" translate-options))
    (set! compile-options (append pic-options '("-DC_SHARED") compile-options))
    (set! link-options
      (cons (cond
             (osx (if lib "-dynamiclib" "-bundle"))
             (else "-shared")) link-options))
    (set! shared #t) )

  (define (use-private-repository)
    (set! compile-options (cons "-DC_PRIVATE_REPOSITORY" compile-options))
    (when osx
      ;; needed for C_path_to_executable (see chicken.h):
      (set! link-options (cons "-framework CoreFoundation" link-options))))

  (let loop ((args args))
    (cond [(null? args)
	   ;; Builtin search directory options do not override explicit options
           (set! compile-options (append compile-options builtin-compile-options))
           (set! link-options (append link-options (builtin-link-options)))
	   ;;
	   (when inquiry-only
	     (when show-cflags (print* (compiler-options) #\space))
	     (when show-ldflags (print* (linker-options) #\space))
	     (when show-libs (print* (linker-libraries) #\space))
	     (newline)
	     (exit) )
	   (cond [(null? scheme-files)
		  (when (and (null? c-files) 
			     (null? object-files))
		    (quit "no source files specified") )
		  (let ((f0 (last (if (null? c-files) object-files c-files))))
		    (unless target-filename
		      (set! target-filename 
			(if shared
			    (pathname-replace-extension f0 shared-library-extension)
			    (pathname-replace-extension f0 executable-extension) ) ) ) ) ]
		 [else
		  (when (and shared (not embedded))
		    (set! translate-options (cons "-dynamic" translate-options)) )
		  (unless target-filename
		    (set! target-filename
		      (if shared
			  (pathname-replace-extension (first scheme-files) shared-library-extension)
			  (pathname-replace-extension (first scheme-files) executable-extension) ) ) )
		  (run-translation) ] )
	   (when (and deploy (not shared))
	     (use-private-repository))
	   (unless translate-only 
	     (run-compilation)
	     (unless compile-only
	       (when (member target-filename scheme-files)
		 (printf "Warning: output file will overwrite source file `~A' - renaming source to `~A.old'~%"
			 target-filename target-filename)
		 (command 
		  (sprintf
		      "~A ~A ~A" 
		      (if windows-shell "move" "mv")
		    (quotewrap target-filename)
		    (quotewrap (string-append target-filename ".old")))))
	       (run-linking)) ) ]
	  [else
	   (let* ([arg (car args)]
		  [rest (cdr args)]
		  [s (string->symbol arg)] )
	     (case s
	       [(-help --help)
		(usage)
		(exit) ]
	       [(-release)
		(print (chicken-version))
		(exit) ]
	       [(-version)
		(system (sprintf translator " -version"))
		(exit) ]
	       [(-c++) 
		(set! cpp-mode #t)
		(when osx (set! compile-options (cons "-no-cpp-precomp" compile-options))) ]
	       [(-objc) 
		(set! objc-mode #t) ]
	       [(-static) 
		(set! translate-options
		  (cons* "-feature" "chicken-compile-static" translate-options))
		(set! static #t) ]
	       [(-static-libs) 
		(set! translate-options
		  (cons* "-feature" "chicken-compile-static" translate-options))
		(set! static-libs #t) ]
	       [(-cflags)
		(set! inquiry-only #t) 
		(set! show-cflags #t) ]
	       [(-ldflags)
		(set! inquiry-only #t)
		(set! show-ldflags #t) ]
	       [(-cc-name) (print compiler) (exit 0)]
	       [(-cxx-name) (print c++-compiler) (exit 0)]
	       [(-ld-name) (print linker) (exit 0)]
	       [(-home) (print home) (exit 0)]
	       [(-libs)
		(set! inquiry-only #t)
		(set! show-libs #t) ]
	       ((-v -verbose)
		(when (number? verbose)
		  (set! compile-options (cons* "-v" "-Q" compile-options))
		  (set! link-options (cons "-v" link-options)) )
		(t-options "-verbose")
		(if verbose
		    (set! verbose 2)
		    (set! verbose #t)) )
	       [(-w -no-warnings)
		(set! compile-options (cons "-w" compile-options))
		(t-options "-no-warnings") ]
	       [(|-A| -analyze-only)
		(set! translate-only #t)
		(t-options "-analyze-only") ]
	       [(|-P| -check-syntax)
		(set! translate-only #t)
		(t-options "-check-syntax") ]
	       [(-k) (set! keep-files #t)]
	       [(-c) (set! compile-only #t)]
	       [(-t) (set! translate-only #t)]
	       [(-e -embedded)
		(set! embedded #t)
		(set! compile-options (cons "-DC_EMBEDDED" compile-options)) ]
	       [(-require-extension -R)
		(check s rest)
		(set! required-extensions (append required-extensions (list (car rest))))
		(t-options "-require-extension" (car rest))
		(set! rest (cdr rest)) ]
	       ((-private-repository)
		(use-private-repository))
	       ((-no-elevation)
		(set! generate-manifest #t))
	       [(-gui)
		(set! gui #t)
		(set! compile-options (cons "-DC_GUI" compile-options))
		(when mingw
		  (set! object-files 
		    (cons (make-pathname 
			   INSTALL_SHARE_HOME "chicken.rc"
			   object-extension) 
			  object-files))
		  (set! link-options
		    (cons* "-lkernel32" "-luser32" "-lgdi32" "-mwindows"
			   link-options)))]
	       ((-deploy)
		(set! deploy #t)
		(set! deployed #t))
	       ((-deployed)
		(set! deployed #t))
	       [(-framework)
		(check s rest)
		(when osx 
		  (set! link-options (cons* "-framework" (car rest) link-options)) )
		(set! rest (cdr rest)) ]
	       [(-o -output-file)
		(check s rest)
		(let ([fn (car rest)])
		  (set! rest (cdr rest))
		  (set! target-filename fn) ) ]
	       [(|-O| |-O1|) (set! rest (cons* "-optimize-level" "1" rest))]
	       [(|-O0|) (set! rest (cons* "-optimize-level" "0" rest))]
	       [(|-O2|) (set! rest (cons* "-optimize-level" "2" rest))]
	       [(|-O3|) (set! rest (cons* "-optimize-level" "3" rest))]
	       [(|-O4|) (set! rest (cons* "-optimize-level" "4" rest))]
	       [(|-O5|)
		(set! rest (cons* "-optimize-level" "5" rest))]
	       [(|-d0|) (set! rest (cons* "-debug-level" "0" rest))]
	       [(|-d1|) (set! rest (cons* "-debug-level" "1" rest))]
	       [(|-d2|) (set! rest (cons* "-debug-level" "2" rest))]
	       [(-dry-run) 
		(set! verbose #t)
		(set! dry-run #t)]
	       [(-s -shared -dynamic)
		(shared-build #f) ]
	       [(-dll -library)
		(shared-build #t) ]
	       [(-compiler)
		(check s rest)
		(set! translator (car rest))
		(set! rest (cdr rest)) ]
	       [(-cc)
		(check s rest)
		(set! compiler (car rest))
		(set! rest (cdr rest)) ]
	       [(-cxx)
		(check s rest)
		(set! c++-compiler (car rest))
		(set! rest (cdr rest)) ]
	       [(-ld)
		(check s rest)
		(set! linker (car rest))
		(set! rest (cdr rest)) ]
	       [(|-I|)
		(check s rest)
		(set! rest (cons* "-include-path" (car rest) (cdr rest))) ]
	       [(|-C|)
		(check s rest)
		(set! compile-options (append compile-options (string-split (car rest))))
		(set! rest (cdr rest)) ]
	       [(-strip)
		(set! link-options (append link-options (list "-s")))]
	       [(|-L|)
		(check s rest)
		(set! link-options (append link-options (string-split (car rest))))
		(set! rest (cdr rest)) ]
	       [(-rpath)
		(check s rest)
		(set! rpath (car rest))
		(when (and (memq (build-platform) '(gnu clang))
			   (not mingw) (not osx))
		  (set! link-options (append link-options (list (string-append "-Wl,-R" rpath)))) )
	  	(set! rest (cdr rest)) ]
	       [(-host) #f]
	       [(-) 
		(set! scheme-files (append scheme-files '("-")))
		(unless target-filename
		  (set! target-filename (make-pathname #f "a" executable-extension)))]
	       [else
		(when (eq? s '-to-stdout) 
		  (set! to-stdout #t)
		  (set! translate-only #t) )
		(when (memq s '(-optimize-level -benchmark-mode))
		  (set! compilation-optimization-options best-compilation-optimization-options)
		  (set! linking-optimization-options best-linking-optimization-options) )
		(cond [(assq s shortcuts) => (lambda (a) (set! rest (cons (cadr a) rest)))]
		      [(memq s simple-options) (t-options arg)]
		      [(memq s complex-options) 
		       (check s rest)
		       (let* ((n (car rest))
			      (ns (string->number n)) )
			 (t-options arg n)
			 (set! rest (cdr rest)) ) ]
		      [(and (> (string-length arg) 2) (string=? "-:" (substring arg 0 2)))
		       (t-options arg) ]
		      [(and (> (string-length arg) 1)
			    (char=? #\- (string-ref arg 0)) )
		       (cond [(char=? #\l (string-ref arg 1))
			      (set! link-options (append link-options (list arg))) ]
 			     [(char=? #\L (string-ref arg 1))
 			      (set! link-options (append link-options (list arg))) ]
 			     [(char=? #\I (string-ref arg 1))
 			      (set! compile-options (append compile-options (list arg))) ]
			     [(char=? #\D (string-ref arg 1))
			      (t-options "-feature" (substring arg 2)) ]
			     [(char=? #\F (string-ref arg 1))
			      (when osx
				(set! compile-options (append compile-options (list arg))) ) ]
			     [(and (> (string-length arg) 3) (string=? "-Wl," (substring arg 0 4)))
			      (set! link-options (append link-options (list arg))) ]
			     [(> (string-length arg) 2)
 			      (let ([opts (cdr (string->list arg))])
 				(if (null? (lset-difference char=? opts short-options))
 				    (set! rest
 				      (append (map (lambda (o) (string-append "-" (string o))) opts) rest) )
 				    (quit "invalid option `~A'" arg) ) ) ]
			     [else (quit "invalid option `~A'" s)] ) ]
		      [(file-exists? arg)
		       (let-values ([(dirs name ext) (decompose-pathname arg)])
			 (cond [(not ext)
				(set! scheme-files (append scheme-files (list arg)))]
			       [(member ext '("h" "c"))
				(set! c-files (append c-files (list arg))) ]
			       ((string-ci=? ext "rc")
				(set! rc-files (append rc-files (list arg))) )
			       [(member ext '("cpp" "C" "cc" "cxx" "hpp"))
				(when osx (set! compile-options (cons "-no-cpp-precomp" compile-options)))
				(set! cpp-mode #t)
				(set! c-files (append c-files (list arg))) ]
			       [(member ext '("m" "M" "mm"))
				(set! objc-mode #t)
				(set! c-files (append c-files (list arg))) ]
			       [(or (string=? ext object-extension)
				    (string=? ext library-extension) )
				(set! object-files (append object-files (list arg))) ]
			       [else (set! scheme-files (append scheme-files (list arg)))] ) ) ]
		      [else
		       (let ([f2 (string-append arg ".scm")])
			 (if (file-exists? f2)
			     (set! rest (cons f2 rest))
			     (quit "file `~A' does not exist" arg) ) ) ] ) ] )
	     (loop rest) ) ] ) ) )


;;; Translate all Scheme files:

(define (run-translation)
  (for-each
   (lambda (f)
     (let ([fc (pathname-replace-extension
		(if (= 1 (length scheme-files))
		    target-filename
		    f)
		(cond (cpp-mode "cpp")
		      (objc-mode "m")
		      (else "c") ) ) ] )
       (command
	(string-intersperse 
	 (cons* translator (quotewrap f) 
		(append 
		 (if to-stdout 
		     '("-to-stdout")
		     `("-output-file" ,(quotewrap fc)) )
		 (if (##sys#fudge 13)
		     '("-:d")
		     '())
		 (map quote-option
		      (append 
		       extra-features
		       translate-options 
		       (cond (cpp-mode '("-feature" "chicken-scheme-to-c++"))
			     (objc-mode '("-feature" "chicken-scheme-to-objc"))
			     (else '()))
		       translation-optimization-options)) ) )
	 " ") )
       (set! c-files (append (list fc) c-files))
       (set! generated-c-files (append (list fc) generated-c-files))))
   scheme-files)
  (unless keep-files (for-each $delete-file generated-scheme-files)) )


;;; Compile all C/C++  and .rc files:

(define (run-compilation)
  (let ((ofiles '()))
    (for-each
     (lambda (f)
       (let ([fo (pathname-replace-extension f object-extension)])
	 (command
	  (string-intersperse
	   (list (cond (cpp-mode c++-compiler)
		       (else compiler) )
		 (quotewrap f)
		 (string-append compile-output-flag (quotewrap fo)) 
		 compile-only-flag
		 (if (and cpp-mode (string=? "g++" c++-compiler))
		     "-Wno-write-strings"
		     "")
		 (compiler-options) ) ) )
	 (set! generated-object-files (cons fo generated-object-files))
	 (set! ofiles (cons fo ofiles))))
     c-files)
    (when (and generate-manifest (eq? 'windows (software-type)))
      (let ((rcf (pathname-replace-extension target-filename "rc")))
	(create-win-manifest (pathname-file target-filename) rcf)
	(set! rc-files (cons rcf rc-files))
	(set! generated-rc-files (cons rcf generated-rc-files))))
    (for-each
     (lambda (f)
       (let ((fo (string-append f "." object-extension)))
	 (command
	  (string-intersperse
	   (list rc-compiler (quotewrap f) (quotewrap fo))))
	 (set! generated-object-files (cons fo generated-object-files))
	 (set! ofiles (cons fo ofiles))))
     rc-files)
    (set! object-files (append (reverse ofiles) object-files)) ; put generated object files first
    (unless keep-files 
      (for-each $delete-file generated-c-files)
      (for-each $delete-file generated-rc-files))))

(define (compiler-options)
  (string-intersperse
   (map quote-option
	(append
	 (if (or static static-libs) '() nonstatic-compilation-options)
	 compilation-optimization-options
	 compile-options) ) ) )


;;; Link object files and libraries:

(define (run-linking)
  (let* ((files (map quotewrap object-files))
	 (target (quotewrap target-filename))
	 (targetdir #f))
    (when deploy
      (set! targetdir (pathname-strip-extension target-filename))
      (when (and osx gui)
	(set! targetdir (make-pathname #f targetdir "app"))
	(command (sprintf "mkdir -p ~a" (quotewrap (make-pathname targetdir "Contents/MacOS"))))
	(command (sprintf "mkdir -p ~a" (quotewrap (make-pathname targetdir "Contents/Resources")))))
      (set! target-filename
	(make-pathname
	 targetdir
	 (if (and osx gui)
	     (string-append "Contents/MacOS/" (pathname-file target-filename))
	     (pathname-file target-filename))))
      (set! target (quotewrap target-filename))
      (unless (directory-exists? targetdir)
	(when verbose
	  (print "mkdir " targetdir))
	(create-directory targetdir)))
    (command
     (string-intersperse 
      (cons* (cond (cpp-mode c++-linker)
		   (else linker) )
	     (append
	      files
	      (list (string-append link-output-flag (quotewrap target-filename))
		    (linker-options)
		    (linker-libraries) ) ) ) ) )
    (when (and osx (or (not cross-chicken) host-mode))
      (command
       (string-append
	"install_name_tool -change " libchicken ".dylib "
	(quotewrap 
	 (let ((lib (string-append libchicken ".dylib")))
	   (if deployed
	       (make-pathname "@executable_path" lib)
	       (make-pathname
		(lib-path)
		lib))))
	" " 
	target) )
      (when (and gui (not deploy))
	(rez target)))
    (when (and deploy (not (or static static-libs)))
      (copy-libraries 
       (if (and osx gui)
	   (make-pathname targetdir "Contents/MacOS")
	   targetdir))
      (when (and osx gui)
	(create-mac-bundle
	 (pathname-file target-filename)
	 targetdir)))
    (unless keep-files (for-each $delete-file generated-object-files)) ) )

(define (lib-path)
  (prefix "" 
	  "lib"
	  (if win
	      INSTALL_BIN_HOME
	      (if host-mode
		  INSTALL_LIB_HOME
		  TARGET_RUN_LIB_HOME))))

(define (target-lib-path)
  (let ((tdir TARGET_LIB_HOME))
    (if (and (not (string=? tdir ""))
	     cross-chicken
	     (not host-mode))
	tdir
	(lib-path))))

(define (copy-libraries targetdir)
  (let ((lib (make-pathname
	      (target-lib-path) 
	      libchicken
	      (cond (osx "dylib")
		    ((or win cygwin) "dll")
		    (else (string-append
                           "so."
                           (number->string BINARY_VERSION)))))))
    (copy-files lib targetdir)))

(define (copy-files from to)
  (command
   (sprintf "~a ~a ~a"
     (if windows-shell 
	 "copy /Y"
	 "cp")
     (quotewrap from)
     (quotewrap to))))

(define (linker-options)
  (string-append
   (string-intersperse
    (append linking-optimization-options link-options))
   (if (and static (not mingw) (not osx)) " -static" "") ) )

(define (linker-libraries)
  (string-intersperse
   (append
    (if (or static static-libs)
        library-files
        shared-library-files)
    (if (or static static-libs)
        (list extra-libraries)
        (list extra-shared-libraries)))))


;;; Helper procedures:

(define-constant +hairy-chars+ '(#\\ #\#))

(define (cleanup s)
  (let* ((q #f)
	 (s (list->string
	     (let fold ([s (string->list s)])
	       (if (null? s) 
		   '()
		   (let ([c (car s)])
		     (cond ((memq c +hairy-chars+) (cons* #\\ c (fold (cdr s))))
			   (else
			    (when (char-whitespace? c) (set! q #t))
			    (cons c (fold (cdr s))) ) ) ) ) ) ) ) )
    (if q 
	(string-append "\"" (string-translate* s '(("\"" . "\\\""))) "\"")
	s) ) )

(define (quote-option x)
  (cond ((string-any (cut char=? #\" <>) x) x)
	((string-any (lambda (c)
		       (or (char-whitespace? c) (memq c +hairy-chars+)) )
		     x)
	 (cleanup x))
	(else x) ))

(define last-exit-code #f)

(define ($system str)
  (when verbose (print str))
  (let ((str (if windows-shell
		 (string-append "\"" str "\"")
		 str)))
    (let ((raw-exit-code (if dry-run 0 (system str))))
      (unless (zero? raw-exit-code)
	(printf "\nError: shell command terminated with non-zero exit status ~S: ~A~%" raw-exit-code str))
      (set! last-exit-code
	(if (zero? raw-exit-code) 0 1))
      last-exit-code)))

(define (command str)
  (unless (zero? ($system str))
    (exit last-exit-code)))

(define ($delete-file str)
  (when verbose 
    (print "rm " str) )
  (unless dry-run (delete-file str) ))

(define (rez file)
  ;; see also: http://www.cocan.org/getting_started_with_ocaml_on_mac_os_x
  (command 
   (sprintf "/Developer/Tools/Rez -t APPL -o ~a ~a"
     (quotewrap file)
     (quotewrap (make-pathname home "mac.r")))))

(define (create-mac-bundle prg dname)
  (let* ((d0 (make-pathname dname "Contents"))
	 (d (make-pathname dname "Contents/MacOS"))
	 (d2 (make-pathname dname "Contents/Resources")))
    (let ((icons (make-pathname d2 "CHICKEN.icns")))
      (unless (file-exists? icons)
	(copy-files 
	 (make-pathname home "chicken/CHICKEN.icns") 
	 d2)))
    (let ((pl (make-pathname d0 "Info.plist")))
      (unless (file-exists? pl)
	(when verbose (print "generating " pl))
	(with-output-to-file pl
	  (cut print #<#EOF
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist SYSTEM "file://localhost/System/Library/DTDs/PropertyList.dtd">
<plist version="0.9">
<dict>
	<key>CFBundlePackageType</key>
	<string>APPL</string>
	<key>CFBundleIconFile</key>
	<string>CHICKEN.icns</string>
        <key>CFBundleGetInfoString</key>
	<string>Created by CHICKEN</string>
	<key>CFBundleSignature</key>
	<string>????</string>
	<key>CFBundleExecutable</key>
	<string>#{prg}</string>
</dict>
</plist>
EOF
)))
      d)))

(define (create-win-manifest prg rcfname)
  (when verbose (print "generating " rcfname))
  (with-output-to-file rcfname
    (lambda ()
      (print #<#EOF
1 24 MOVEABLE PURE
BEGIN
  "<?xml version=""1.0"" encoding=""UTF-8"" standalone=""yes""?>\r\n"
  "<assembly xmlns=""urn:schemas-microsoft-com:asm.v1"" manifestVersion=""1.0"">\r\n"
  "  <assemblyIdentity version=""1.0.0.0"" processorArchitecture=""*"" name=""#{prg}"" type=""win32""/>\r\n"
  "  <ms_asmv2:trustInfo xmlns:ms_asmv2=""urn:schemas-microsoft-com:asm.v2"">\r\n"
  "    <ms_asmv2:security>\r\n"
  "      <ms_asmv2:requestedPrivileges>\r\n"
  "        <ms_asmv2:requestedExecutionLevel level=""asInvoker"" uiAccess=""false""/>\r\n"
  "      </ms_asmv2:requestedPrivileges>\r\n"
  "    </ms_asmv2:security>\r\n"
  "  </ms_asmv2:trustInfo>\r\n"
  "</assembly>\r\n"
END
EOF
) ) ) )


;;; Run it:

(run
 (append 
  (string-split (or (get-environment-variable "CSC_OPTIONS") "")) 
  arguments))
