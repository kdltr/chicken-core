;;;; utils.scm - Utilities for scripting and file stuff
;
; Copyright (c) 2008-2015, The CHICKEN Team
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
  (unit utils)
  (uses data-structures posix files)
  (fixnum)
  (disable-interrupts) )

(module chicken.utils
  (compile-file
   compile-file-options
   system*
   qs)

(import scheme chicken)
(import chicken.data-structures
	chicken.files
	chicken.foreign
	chicken.format
	chicken.posix)

(include "common-declarations.scm")


;;; Like `system', but allows format-string and bombs on nonzero return code:

(define system*
  (lambda (fstr . args)
    (let* ([str (apply sprintf fstr args)]
	   [n (system str)] )
      (unless (zero? n)
	(##sys#error "shell invocation failed with non-zero return status" str n) ) ) ) )


;;; Quote string for shell

(define (qs str #!optional (platform (build-platform)))
  (let ((delim (if (eq? platform 'mingw32) #\" #\'))
	(escaped (if (eq? platform 'mingw32) "\"\"" "'\\''")))
    (string-append
     (string delim)
     (string-intersperse
      (map (lambda (c)
	     (cond
	      ((char=? c delim) escaped)
	      ((char=? c #\nul) (error 'qs "NUL character can not be represented in shell string" str))
	      (else (string c))))
	   (string->list str))
      "")
     (string delim))))


;;; Compile and load file

(define compile-file-options (make-parameter '("-O2" "-d2")))

(define compile-file
  (let ((csc (foreign-value "C_CSC_PROGRAM" c-string))
	(load-file load)
	(path (foreign-value "C_INSTALL_BIN_HOME" c-string)) )
    (lambda (filename #!key options output-file (load #t) verbose)
      (let* ((cscpath (or (file-exists? (make-pathname path csc)) "csc"))
	     (tmpfile (and (not output-file) (create-temporary-file "so")))
	     (crapshell (eq? (build-platform) 'mingw32))
	     (cmd (sprintf "~a~a -s ~a ~a -o ~a~a" 
		    (if crapshell "\"" "")
		    (qs cscpath)
		    (string-intersperse
		     (or options
			 (compile-file-options)))
		    (qs filename)
		    (qs (or output-file tmpfile))
		    (if crapshell "\"" ""))))
	(when verbose (print "  " cmd))
	(let ((status (system cmd)))
	  (cond ((zero? status)
		 (unless output-file 
		   (on-exit
		    (lambda ()
		      (handle-exceptions ex #f (delete-file* tmpfile)))))
		 (when load
		   (let ((f (or output-file tmpfile)))
		     (handle-exceptions ex
			 (begin
			   (delete-file* f)
			   (abort ex))
		       (load-file f)
		       f))))
		(else #f)))))))

)
