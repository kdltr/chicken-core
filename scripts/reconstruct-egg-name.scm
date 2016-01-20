;;;; reconstruct-egg-name.scm - add "egg-name" infolist properties to all installed eggs
;
; by Kon Lovett
; (minimally modified by felix)


(use extras files posix miscmacros)

;; Write the elements of the list `ls' to the output-port or output-file,
;; using
;; the `writer' procedure. `writer' is a (procedure (* output-port)).
;;
;; (add `newline?' param for write #\newline after every element?)
(define (write-file ls #!optional (file-or-port (current-output-port)) (writer write))
  (let ((port (if (port? file-or-port) file-or-port
                (open-output-file file-or-port) ) ) )
    (dynamic-wind
      void
      (lambda () (for-each (cut writer <> port) ls))
      (lambda () (unless (port? file-or-port) (close-output-port port)))) )
      )

#;
(define (write-file ls #!optional (file-or-port (current-output-port)) (writer write))
  (let* ((port (if (port? file-or-port) file-or-port
                 (open-output-file file-or-port) ) )
         (closit (lambda () (unless (port? file-or-port) (close-output-port
         port)))) )
    (handle-exceptions exn
        (begin (closit) (abort exn))
      (for-each (cut writer <> port) ls))
    (closit) ) )

(define-constant +info-extn+ "setup-info")

(define (info-filenames #!optional (dir (repository-path)))
  (let ((cd (current-directory)))
    (current-directory dir)
    (begin0
      (map pathname-file (glob (make-pathname '() "*" +info-extn+)))
      (current-directory cd) ) ) )

(define (get-info eggnam #!optional (dir (repository-path)))
  (car (call-with-input-file (make-pathname dir eggnam +info-extn+) read-all)))

(define (put-info info eggnam #!optional (dir (repository-path)))
  (let ((tmpfil (create-temporary-file)))
    (write-file (list info) tmpfil)
    (file-move tmpfil (make-pathname dir eggnam +info-extn+) #t) ) )

(define (update-info info eggnam)
  (if (assq 'egg-name info)
      info
      (cons
       `(egg-name ,eggnam)
       info)) )

(define (update-info-file eggnam #!optional (dir (repository-path)))
  (and-let* ((info (update-info (get-info eggnam dir) eggnam)))
    (put-info info eggnam dir)
    #t ) )

(define (main svnroot)
  (let ((eggdir svnroot)
        (repdir (repository-path)) )
    (newline)
    (print "    Local SVN Repository: " eggdir)
    (print "Installed Egg Repository: " repdir)
    (newline)
    (let* ((egdrnms (directory eggdir))
           (siflnms (info-filenames repdir))
           (eggnams (foldl (lambda (a e)
                             (if (member e siflnms) (cons e a) a))
                           '()
                           egdrnms)))
    (for-each
      (lambda (eggnam)
        (print eggnam)
        (unless (update-info-file eggnam)
          (print "Warning: no version detected") ) )
      eggnams ) ) ) )

(apply main (command-line-arguments))
