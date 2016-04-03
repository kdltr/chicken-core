;;;; egg-info processing and compilation


(import (chicken))
(import (chicken data-structures))
(import (chicken pretty-print))
(import (chicken files))


(define valid-items
  '(synopsis authors category license version dependencies files
    source-file csc-options test-dependencies destination linkage
    build-dependencies components foreign-dependencies link-options))

(define nested-items 
  '(components))

(define named-items
  '(extension program data c-include scheme-include))

(define default-extension-options '())
(define default-program-options '())
(define default-static-program-link-options '())
(define default-dynamic-program-link-options '())
(define default-static-extension-link-options '())
(define default-dynamic-extension-link-options '())
(define default-extension-linkage '(static dynamic))
(define default-program-linkage '(dynamic))
(define unix-executable-extension "")
(define windows-executable-extension ".exe")
(define unix-object-extension ".o")
(define windows-object-extension ".obj")


(define (validate-egg-info info)
  (unless (list? info) 
    (error "egg-information has invalid structure"))
  (for-each
    (lambda (item)
      (cond ((not (and (list? item) (pair? item) (symbol? (car item))))
              (error "egg-information item has invalid structure" item))
            ((not (memq (car item) valid-items))
              (error "invalid item" item))
            ((and (memq (car item) named-items) (not (symbol? (cadr item))))
              (error "missing name for item" item))
            ((memq (car item) nested-items)
              (validate-egg-info 
                (if (memq (car item) named-items) (cddr item) (cdr item))))))
    info))

(define (compile-egg-info info platform)
  (let ((exts '())
        (prgs '())
        (data '())
        (cinc '())
        (scminc '())
        (target #f)
        (src #f)
        (files '())
        (ifiles '())
        (link '())
        (dest #f)
        (deps '())
        (lopts '())
        (opts '())
        (objext #f)
        (exeext #t))
    (define (check-target t lst)
      (when (member t lst)
        (error "target multiply defined" t))
      t)
    (define (addfiles . files)
      (set! ifiles (append files ifiles))
      files)
    (define (compile-component info)
      (case (car info)
        ((extension)
          (fluid-let ((target (check-target (cadr info) exts))
                      (deps '())
                      (src #f)
                      (link default-extension-linkage)
                      (lopts '())
                      (opts '()))
            (for-each compile-extension/program (cddr info))
            (addfiles 
              (if (memq 'static link) (conc target objext) '())
              (if (memq 'dynamic link) (conc target ".so") '())
              (conc target ".import.so")) ; assumes import-lib is always compiled?
            (set! exts (cons (list target deps src opts lopts link) exts))))
        ((data)
          (fluid-let ((target (check-target (cadr info) data))
                      (dest #f)
                      (files '()))
            (for-each compile-data/include (cddr info))
            (let ((dest (normalize-pathname (conc dest "/"))))
              (for-each addfiles (map (cut conc dest <>) files)))
            (set! data (cons (list target '() files dest) data))))
        ((c-include)
          (fluid-let ((target (check-target (cadr info) cinc))
                      (dest #f)
                      (files '()))
            (for-each compile-data/include (cddr info))
            (let ((dest (normalize-pathname (conc dest "/"))))
              (for-each addfiles (map (cut conc dest <>) files)))
            (set! cinc (cons (list target '() files dest) cinc))))
        ((scheme-include)
          (fluid-let ((target (check-target (cadr info) scminc))
                      (dest #f))
                      (files '()))
            (for-each compile-data/include (cddr info))
            (let ((dest (normalize-pathname (conc dest "/"))))
              (for-each addfiles (map (cut conc dest <>) files)))
            (set! scminc (cons (list target '() files dest) scminc)))
        ((program)
          (fluid-let ((target (check-target (cadr info) prgs))
                      (deps '())
                      (src #f)
                      (link default-program-linkage)
                      (lopts '())
                      (opts '()))
            (for-each compile-extension/program (cddr info))
            (addfiles (conc target exeext))
            (set! prgs (cons (list target deps src opts lopts link) prgs))))))
    (define (compile-extension/program info)
      (case (car info)
        ((linkage) 
         (set! link (cdr info)))
        ((csc-options) 
         (set! opts (append opts (cdr info))))
        ((link-options)
         (set! lopts (append lopts (cdr info))))
        ((source-file)
         (set! src (->string (arg info 1 name?))))
        ((dependencies)
         (set! deps (append deps (map ->dep (cdr info)))))))
    (define (compile-data/include info)
      (case (car info)
        ((destination)
         (set! dest (->string (arg info 1 name?))))
        ((files) 
         (set! files (append files (map ->string (cdr info)))))))
    (define (->dep x)
      (if (name? x) x (error "invalid dependency" x)))
    (define (compile info)
      (case (car info)
        ((components) (for-each compile-component (cdr info)))))
    (define (arg info n #!optional (pred (constantly #t)))
      (when (< (length info) n)
        (error "missing argument" info n))
      (let ((x (list-ref info n)))
        (unless (pred x)
          (error "argument has invalid type" x))
        x))
    (define (name? x) (or (string? x) (symbol? x)))
    (define dep=? equal?)
    (define (filter pred lst)
      (cond ((null? lst) '())
            ((pred (car lst)) (cons (car lst) (filter pred (cdr lst))))
            (else (filter pred (cdr lst)))))
    (define (filter-deps name deps)
      (filter (lambda (dep)
                (and (symbol? dep)
                     (or (assq dep exts)
                         (assq dep data)
                         (assq dep cinc)
                         (assq dep scminc)
                         (error "unknown component dependency" dep name))))
              deps))
    (case platform
      ((unix) 
        (set! objext unix-object-extension)
        (set! exeext unix-executable-extension))
      ((windows) 
        (set! objext windows-object-extension)
        (set! exeext windows-executable-extension)))
    ;; collect information
    (for-each compile info)
    ;; sort topologically, by dependencies
    (let ((order (reverse 
                   (topological-sort          
                     (map (lambda (dep)
                            (cons (car dep) 
                                  (filter-deps (car dep) (cadr dep))))
                          exts)
                      dep=?))))
      ;; generate + return build/install commands
      (values
        ;; build commands
        (append 
          (foldr
            (lambda (t cmds)
              (let* ((data (assq t exts))
                     (link (list-ref data 5)))
                (append
                  (if (memq 'static link) 
                      `((compile-static-extension ,@data))
                      '())
                  (if (memq 'dynamic link) 
                      `((compile-dynamic-extension ,@data))
                      '())
                  `((compile-import-library ,@data))
                  cmds)))
            '() order)
          (map (lambda (prg) `(compile-program ,prg)) prgs))
        ;; installation commands
        (append
          (map (lambda (ext) `(install-extension ,@ext)) exts)
          (map (lambda (prg) `(install-program ,@prg)) prgs)
          (map (lambda (data) `(install-data ,@data)) data)
          (map (lambda (cinc) `(install-c-include ,@cinc)) cinc)
          (map (lambda (scminc) `(install-scheme-include ,@scminc)) scminc))
        ;; augmented egg-info
        (cons `(installed-files ,@ifiles) info)))))


;;; UNIX shell code generation

(define (gen-compile-unix-extension name deps src opts lopts link)
  ...)

(define (gen-compile-unix-program name deps src opts lopts link)
  ...)

(define (gen-install-unix-extension name deps src opts lopts link)
  ...)

(define (gen-install-unix-program name deps src opts lopts link)
  ...)

(define (gen-install-unix-data name deps files dest)
  ...)

(define (gen-install-unix-c-include name deps files dest)
  ...)

(define (gen-install-unix-scheme-include name deps files dest)
  ...)


;;; Windows batch code generation

(define (gen-compile-windows-extension name deps src opts lopts link)
  ...)

(define (gen-compile-windows-program name deps src opts lopts link)
  ...)

(define (gen-install-windows-extension name deps src opts lopts link)
  ...)

(define (gen-install-windows-program name deps src opts lopts link)
  ...)

(define (gen-install-windows-data name deps files dest)
  ...)

(define (gen-install-windows-c-include name deps files dest)
  ...)

(define (gen-install-windows-scheme-include name deps files dest)
  ...)


;;; platform-independent part of code generation

(define generators
  `((compile-static-extension (unix ,gen-compile-unix-static-extension) 
                              (windows ,gen-compile-windows-static-extension))
    (compile-dynamic-extension (unix ,gen-compile-unix-dynamic-extension) 
                              (windows ,gen-compile-windows-dynamic-extension))
    (compile-import-library (unix ,gen-compile-unix-import-library) 
                              (windows ,gen-compile-windows-import-library))
    (compile-program (unix ,gen-compile-unix-program) 
                     (windows ,gen-compile-windows-program))
    (install-extension (unix ,gen-install-unix-extension)
                       (windows ,gen-install-windows-extension))
    (install-program (unix ,gen-install-unix-program)
                     (windows ,gen-install-windows-program))
    (install-data (unix ,gen-install-unix-data)
                  (windows ,gen-install-windows-data))
    (install-c-include (unix ,gen-install-unix-c-include)
                       (windows ,gen-install-windows-c-include))
    (install-scheme-include (unix ,gen-install-unix-scheme-include)
                            (windows ,gen-install-windows-scheme-include))))

(define (generate-shell-commands platform cmds dest prefix suffix)
  (with-output-to-file dest
    (lambda ()
      (prefix)
      (for-each
        (lambda (cmd)
          (cond ((assq cmd generators) =>
                  (lambda (gen)
                    (cond ((assq platform (cdr gen)) =>
                            (lambda (a) (apply (cdr a) (cdr cmd))))
                          (else (error "invalid platform" platform)))))
                (else (error "invalid command" cmd))))
        cmds)
      (suffix))))
                        

;;

(set! hyde (with-input-from-file "hyde.egg" read))
(pp (receive (compile-egg-info hyde)))
