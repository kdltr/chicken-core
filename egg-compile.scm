;;;; egg-info processing and compilation


(define valid-items
  '(synopsis authors category license version dependencies files
    source-file csc-options test-dependencies destination linkage
    build-dependencies components foreign-dependencies link-options
    custom-bulild target host platform doc-from-wiki extension program
    data))  

(define nested-items 
  '(components target host extension program data))

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

(define +unix-executable-extension+ "")
(define +windows-executable-extension+ ".exe")
(define +unix-object-extension+ ".o")
(define +windows-object-extension+ ".obj")


;;; validate egg-information tree

(define (validate-egg-info info)
  (unless (list? info) 
    (error "egg-information has invalid structure"))
  (for-each
    (lambda (item)
      (unless (and (list? item) (pair? item) (symbol? (car item)))
        (error "egg-information item has invalid structure" item))
      (when (and (memq (car item) named-items) (not (symbol? (cadr item))))
        (error "missing name for item" item))
      (if (memq (car item) nested-items)
          (validate-egg-info (if (memq (car item) named-items)
                                 (cddr item) 
                                 (cdr item)))
          (unless (memq (car item) valid-items)
             (error "invalid item" item))))
    info)
  info)


;;; load egg-info from file and perform validation

(define (load-egg-info fname #!optional (validate #t))
  (with-input-from-file fname
    (lambda () 
      (let ((info (read)))
        (if validate
            (validate-egg-info info)
            info)))))


;;; lookup specific entries in egg-information

(define (get-egg-property info prop #!optional default)
  (let ((p (assq prop info)))
    (or (and p (cadr p)) default)))


;;; some utilities

(define (object-extension platform)
  (case platform
    ((unix) +unix-object-extension+)
    ((windows) +windows-object-extension+)))

(define (executable-extension platform)
  (case platform
     ((unix) +unix-executable-extension+)
     ((windows) +windows-executable-extension+)))

(define (copy-directory-command platform)
  (case platform
    ((unix) "cp")
    ((windows) "xcopy /y")))

(define (mkdir-command platform)
  (case platform
    ((unix) "mkdir -p")
    ((windows) "mkdir")))

(define install-command copy-directory-command)

(define (destination-repository mode)
  (case mode
    ((target) target-repo)
    ((host) host-repo)))

(define (uses-compiled-import-library? mode)
  (not (and (eq? mode 'host) staticbuild)))


;;; compile an egg-information tree into abstract build/install operations

(define (compile-egg-info info platform mode)
  (let ((exts '())
        (prgs '())
        (data '())
        (cinc '())
        (scminc '())
        (target #f)
        (src #f)
        (files '())
        (ifiles '())
        (cbuild #f)
        (link '())
        (dest #f)
        (deps '())
        (lopts '())
        (opts '())
        (objext (object-extension platform))
        (exeext (executable-extension platform)))
    (define (check-target t lst)
      (when (member t lst)
        (error "target multiply defined" t))
      t)
    (define (addfiles . files)
      (set! ifiles (append files ifiles))
      files)
    (define (compile-component info)
      (case (car info)
        ((target) (when (eq? mode 'target) (for-each compile-component (cdr info))))
        ((host) (when (eq? mode 'host) (for-each compile-component (cdr info))))
        ((extension)
          (fluid-let ((target (check-target (cadr info) exts))
                      (deps '())
                      (src #f)
                      (cbuild #f)
                      (link default-extension-linkage)
                      (lopts '())
                      (opts '()))
            (for-each compile-extension/program (cddr info))
            (let ((dest (destination-repository mode)))
              (addfiles 
                (if (memq 'static link) (conc dest "/" target objext) '())
                (if (memq 'dynamic link) (conc dest "/" target ".so") '())
                (if (uses-compiled-import-library? mode)
                    (conc dest "/" target ".import.so")
                    (conc dest "/" target ".import.scm"))))
            (set! exts 
              (cons (list target dependencies: deps source: src options: opts 
                          link-options: lopts linkage: link custom: cbuild
                          mode: mode)                         
                    exts))))
        ((data)
          (fluid-let ((target (check-target (cadr info) data))
                      (dest #f)
                      (files '()))
            (for-each compile-data/include (cddr info))
            (let* ((dest (or dest 
                             (if (eq? mode 'target) target-sharedir host-sharedir)))
                   (dest (normalize-pathname (conc dest "/"))))
              (for-each addfiles (map (cut conc dest <>) files)))
            (set! data
              (cons (list target dependencies: '() files: files 
                          destination: dest mode: mode) 
                    data))))                      
        ((c-include)
          (fluid-let ((target (check-target (cadr info) cinc))
                      (dest #f)
                      (files '()))
            (for-each compile-data/include (cddr info))
            (let* ((dest (or dest 
                             (if (eq? mode 'target) target-incdir host-incdir)))
                   (dest (normalize-pathname (conc dest "/"))))
              (for-each addfiles (map (cut conc dest <>) files)))
            (set! cinc
              (cons (list target dependencies: '() files: files 
                          destination: dest mode: mode) 
                    cinc))))            
        ((scheme-include)
          (fluid-let ((target (check-target (cadr info) scminc))
                      (dest #f))
                      (files '()))
            (for-each compile-data/include (cddr info))
            (let* ((dest (or dest
                             (if (eq? mode 'target) target-sharedir host-sharedir)))
                   (dest (normalize-pathname (conc dest "/"))))
              (for-each addfiles (map (cut conc dest <>) files)))
            (set! scminc 
              (cons (list target dependencies: '() files: files 
                          destination: dest mode: mode) 
                    scminc)))           
        ((program)
          (fluid-let ((target (check-target (cadr info) prgs))
                      (deps '())
                      (cbuild #f)
                      (src #f)
                      (link default-program-linkage)
                      (lopts '())
                      (opts '()))
            (for-each compile-extension/program (cddr info))
            (let ((dest (if (eq? mode 'target) target-bindir host-bindir)))
              (addfiles (conc dest "/" target exeext)))
            (set! prgs 
              (cons (list target dependencies: deps source: src options: opts 
                          link-options: lopts linkage: link custom: cbuild
                          mode: mode)                         
                    prgs))))))
    (define (compile-extension/program info)
      (case (car info)
        ((target) 
          (when (eq? mode 'target) (for-each compile-extension/program (cdr info))))
        ((host) 
          (when (eq? mode 'host) (for-each compile-extension/program (cdr info))))
        ((linkage) 
         (set! link (cdr info)))
        ((custom-build)
         (set! cbuild (arg info 1 string?)))
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
        ((target) (when (eq? mode 'target) (for-each compile-data/include (cdr info))))
        ((host) (when (eq? mode 'host) (for-each compile-data/include (cdr info))))
        ((destination)
         (set! dest (->string (arg info 1 name?))))
        ((files) 
         (set! files (append files (map ->string (cdr info)))))))
    (define (->dep x)
      (if (name? x) x (error "invalid dependency" x)))
    (define (compile info)
      (case (car info)
        ((target) (when (eq? mode 'target) (for-each compile (cdr info))))
        ((host) (when (eq? mode 'host) (for-each compile (cdr info))))
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
    ;; collect information
    (for-each compile info)
    ;; sort topologically, by dependencies
    (let ((order (reverse 
                   (topological-sort          
                     (map (lambda (dep)
                            (cons (car dep) 
                                  (filter-deps 
                                    (car dep)                                       
                                    (get-keyword dependencies: (cdr dep)))))
                          exts)
                      dep=?))))
      ;; generate + return build/install commands
      (values
        ;; build commands
        (append 
          (foldr
            (lambda (t cmds)
              (let* ((data (assq t exts))
                     (link (get-keyword linkage: (cdr data))))
                (append
                  (if (memq 'static link) 
                      `((compile-static-extension ,@data))
                      '())
                  (if (memq 'dynamic link) 
                      `((compile-dynamic-extension ,@data))
                      '())
                  (if (uses-compiled-import-library? mode)
                      `((compile-import-library ,@data))
                      '())
                  cmds)))
            '() order)
          (foldr
            (lambda (prg cmds)   
              (let ((link (get-keyword linkage: (cdr prg))))
                (append
                  (if (memq 'static link) 
                       `((compile-static-program ,@prg))
                      '())
                  (if (memq 'dynamic link) 
                      `((compile-dynamic-program ,@prg))
                      '())
                  cmds)))
            '()
            prgs))
        ;; installation commands
        (append
          (append-map
            (lambda (ext)          
              (let ((link (get-keyword linkage: (cdr ext))))
                (if (memq 'static link)
                    `((install-static-extension ,@ext))
                    '())
                (if (memq 'dynamic link)
                    `((install-dynamic-extension ,@ext))
                    '())))
             exts)
          (map (lambda (ext) 
                  (if (uses-compiled-import-library? (get-keyword mode: ext))
                     `(install-import-library ,@ext)
                     `(install-import-library-source ,@ext)))
               exts)
          (map (lambda (prg) `(install-program ,@prg)) prgs)
          (map (lambda (data) `(install-data ,@data)) data)
          (map (lambda (cinc) `(install-c-include ,@cinc)) cinc)
          (map (lambda (scminc) `(install-scheme-include ,@scminc)) scminc))
        ;; augmented egg-info
        (cons `(installed-files ,@ifiles) info)))))


;;; shell code generation - build operations

(define (gen-compile-static-extension name #!key mode platform dependencies source 
                                      (options '()) custom)
  (let ((cmd (or custom 
                 (conc default-csc " -D compiling-extension -c -J -unit " name
                       " -D compiling-static-extension")))
        (out (quotearg (target-file (conc name (object-extension platform)) mode)))
        (src (quotearg (or source (conc name ".scm")))))
    (print (slashify default-builder platform) " " out " " cmd (arglist options) 
           " " src " -o " out " : "
           src (arglist dependencies))))

(define (gen-compile-dynamic-extension name #!key mode platform dependencies mode
                                       source (options '()) (link-options '()) 
                                       custom)
  (let ((cmd (or custom 
                 (conc default-csc " -D compiling-extension -J -s")))
        (out (quotearg (target-file (conc name ".so") mode)))
        (src (quotearg (or source (conc name ".scm")))))
    (print (slashify default-builder platform) " " out " " cmd (arglist options)
           (arglist link-options) " " src " -o " out " : "
           src (arglist dependencies))))

(define (gen-compile-import-library name #!key platform dependencies source mode
                                    (options '()) (link-options '())
                                    custom)
  (let ((cmd (or custom (conc default-csc " -s")))
        (out (quotearg (target-file (conc name ".import.so") mode)))
        (src (quotearg (or source (conc name ".import.scm")))))
    (print (slashify default-builder platform) " " out " " cmd (arglist options)
           (arglist link-options) " " src " -o " out " : "
           src (arglist dependencies))))

(define (gen-compile-dynamic-program name #!key platform dependencies source mode
                                     (options '()) (link-options '())
                                     custom)
  (let ((cmd (or custom default-csc))
        (out (quotearg 
               (target-file (conc name (executable-extension platform)) mode)))
        (src (quotearg (or source (conc name ".scm")))))
    (print (slashify default-builder platform) " " out " " cmd (arglist options)
           (arglist link-options) " " src " -o " out " : "
           src (arglist dependencies))))

(define (gen-compile-static-program name #!key platform dependencies source
                                    (options '()) (link-options '())
                                    custom mode)
  (let ((cmd (or custom (conc default-csc " -static-libs")))
        (out (quotearg 
               (target-file (conc name (executable-extension platform)) mode)))
        (src (quotearg (or source (conc name ".scm")))))
    (print (slashify default-builder platform) " " out " " cmd (arglist options)
           (arglist link-options) " " src " -o " out " : "
           src (arglist dependencies))))


;; installation operations

(define (gen-install-static-extension name #!key platform mode)
  (let* ((cmd (install-command platform))
         (mkdir (mkdir-command platform))
         (ext (object-extension platform))
         (out (quotearg (target-file (conc name ext) mode)))
         (dest (destination-repository mode))
         (dfile (quotearg dest platform)))
    (print mkdir " " dfile)
    (print cmd " " out " " (quotearg (slashify dest "/" name ext) platform))))

(define (gen-install-dynamic-extension name #!key platform mode)
  (let* ((cmd (install-command platform))
         (out (quotearg (target-file (conc name ".so") mode)))
         (ext (object-extension platform))
         (dest (destination-repository mode))
         (dfile (quotearg (slashify dest platform))))
    (print mkdir " " dfile)
    (print cmd " " out " " (quotearg (slashify dest "/" name ".so") platform))))

(define (gen-install-import-library name #!key platform mode)
  (let* ((cmd (install-command platform))
         (out (quotearg (target-file (conc name ".import.so") mode)))
         (dest (destination-repository mode))
         (dfile (quotearg (slashify dest platform))))
    (print mkdir " " dfile)
    (print cmd " " out " " 
           (quotearg (slashify (conc dest "/" name ".import.so") platform)))))

(define (gen-install-import-library-source name #!key platform mode)
  (let* ((cmd (install-command platform))
         (out (quotearg (target-file (conc name ".import.scm") mode)))
         (dest (destination-repository mode))
         (dfile (quotearg (slashify dest platform))))
    (print mkdir " " dfile)
    (print cmd " " out " " 
          (quotearg (slashify (conc dest "/" name ".import.scm") platform)))))

(define (gen-install-program name #!key platform mode)
  (let* ((cmd (install-command platform))
         (ext (executable-extension platform))
         (out (quotearg (target-file (conc name ext) mode)))
         (dest (if (eq? mode 'target) target-bindir host-bindir))
         (dfile (quotearg (slashify dest platform))))
    (print mkdir " " dfile)
    (print cmd " " out " "
           (quotearg (slashify (conc dest "/" name ext) platform)))))

(define (gen-install-data name #!key platform files destination mode)
  (let* ((cmd (install-command platform))
         (dest (or destination (if (eq? mode 'target) target-sharedir host-sharedir)))
         (dfile (quotearg (slashify dest platform))))
    (print mkdir " " dfile)
    (print cmd (arglist files) " " dfile)))

(define (gen-install-c-include name #!key platform deps files dest mode)
  (let* ((cmd (install-command platform))
         (dest (or dest (if (eq? mode 'target) target-incdir host-incdir)))
         (dfile (quotearg (slashify dest platform))))
    (print mkdir " " dfile)
    (print cmd " " (arglist files) " " dfile)))

(define command-table
  `((compile-static-extension ,gen-compile-static-extension)
    (compile-dynamic-extension ,gen-compile-dynamic-extension)
    (compile-dynamic-program ,gen-compile-dynamic-program)
    (compile-static-program ,gen-compile-static-program)
    (install-static-extension ,gen-install-static-extension)
    (install-dynamic-extension ,gen-install-dynamic-extension)
    (install-import-library ,gen-install-import-library)
    (install-import-library-source ,gen-install-import-library-source)
    (install-data ,gen-install-data)
    (compile-import-library ,gen-compile-import-library)
    (install-data ,gen-install-data)
    (install-c-include ,gen-install-c-include)
    (install-scheme-include ,gen-install-data)))  ;; might be extended


;;; Generate shell or batch commands from abstract build/install operations

(define (generate-shell-commands platform cmds dest prefix suffix)
  (with-output-to-file dest
    (lambda ()
      (prefix platform)
      (for-each
        (lambda (cmd)
          (d "~s~%" cmd)
          (cond ((assq (car cmd) command-table)
                  => (lambda (op) 
                       (apply (cadr op) 
                              (cons* (cadr cmd) platform: platform (cddr cmd)))))
                (else (error "invalid command" cmd))))
        cmds)
      (suffix platform))))
                        

;;; affixes for build- and install-scripts

(define ((build-prefix mode name info) platform)
  (case platform
    ((unix)
     (printf #<<EOF
#!/bin/sh~%
EOF
             ))
    ((windows)
     (printf #<<EOF
@echo off
EOF
             ))))

(define ((build-suffix mode name info) platform)
  (case platform
    ((unix)
     (printf #<<EOF
EOF
             ))
    ((windows)
     (printf #<<EOF
EOF
             ))))

(define ((install-prefix mode name info) platform)
  (case platform
    ((unix)
     (printf #<<EOF
#!/bin/sh~%
EOF
             ))
    ((windows)
     (printf #<<EOF
@echo off~%
EOF
             ))))

(define ((install-suffix mode name info) platform)
  (let ((infostr (with-output-to-string (cut pp info)))
        (dest (make-pathname (destination-repository mode) name +egg-info-extension+)))
    (case platform
      ((unix)
       (printf #<<EOF
cat >~a <<ENDINFO
~aENDINFO~%
EOF
               dest infostr))
      ((windows)
       (printf #<<EOF
echo ~a >~a~%
EOF
               (string-intersperse (string-split infostr) "^\n")
               dest)))))


;;; some utilities for mangling + quoting

(define (quotearg str)
  (let ((lst (string->list str)))
    (if (any char-whitespace? lst)
        (string-append "\"" str "\"")
        str)))

(define (slashify str platform)
  (if (eq? platform 'windows)
      (list->string 
        (map (lambda (c) (if (char=? #\/ c) #\\ c)) (string->list str)))
      str))

(define (quote-all str platform)
  (if (and (eq? platform 'windows) 
           (positive? (string-length str))
           (char=? #\" (string-ref str 0)))
      (string-append "\"" str "\"")
      str))

(define (target-file fname mode)
  (if (eq? mode 'target) (string-append fname ".target") fname))

(define (arglist lst)
  (apply conc (map (lambda (x) (conc " " (quotearg x))) lst)))