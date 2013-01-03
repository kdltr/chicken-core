;;;; utils.scm - Utilities for scripting and file stuff
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
  (unit utils)
  (uses extras srfi-13 posix files irregex)
  (fixnum)
  (hide chop-pds)
  (disable-interrupts) )

(include "common-declarations.scm")

(register-feature! 'utils)


;;; Like `system', but allows format-string and bombs on nonzero return code:

(define system*
  (lambda (fstr . args)
    (let* ([str (apply sprintf fstr args)]
	   [n (system str)] )
      (unless (zero? n)
	(##sys#error "shell invocation failed with non-zero return status" str n) ) ) ) )


;;; Read file as string from given filename or port:

(define (read-all . file)
  (let ([file (optional file ##sys#standard-input)])
    (if (port? file)
	(read-string #f file)
	(with-input-from-file file (cut read-string #f) #:binary) ) ) )


;;; Quote string for shell

(define (qs str #!optional (platform (build-platform)))
  (case platform
    ((mingw32)
     (string-append "\"" str "\""))
    (else
     (if (zero? (string-length str))
	 "''"
	 (string-concatenate
	  (map (lambda (c)
		 (if (or (char-whitespace? c)
			 (memq c '(#\# #\" #\' #\` #\´ #\~ #\& #\% #\$ #\! #\* #\;
				   #\< #\> #\\ #\( #\) #\[ #\] #\{ #\} #\?)))
		     (string #\\ c)
		     (string c)))
	       (string->list str)))))))


;;; Compile and load file

(define compile-file-options (make-parameter '("-S" "-O2" "-d2")))

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


;;; Scan lines until regex or predicate matches

(define scan-input-lines
  (lambda (rx #!optional (port ##sys#standard-input))
    (let ((rx (if (procedure? rx)
		  rx
		  (cute irregex-search (irregex rx) <>))))
      (let loop ()
	(let ((ln (read-line port)))
	  (and (not (eof-object? ln))
	       (or (rx ln)
		   (loop))))))))


;; Ask for confirmation

#>
#if defined(_WIN32) && !defined(__CYGWIN__)
# include <windows.h>
# define C_HAS_MESSAGE_BOX 1
static int
C_confirmation_dialog(char *msg, char *caption, int def, int abort)
{
  int d = 0, r;
  int t = abort ? MB_YESNOCANCEL : MB_YESNO;

  switch(def) {
  case 0: d = MB_DEFBUTTON1; break;
  case 1: d = MB_DEFBUTTON2; break;
  case 2: d = MB_DEFBUTTON3;
  }

  r = MessageBox(NULL, msg, caption, t | MB_ICONQUESTION | d);

  switch(r) {
  case IDYES: return 1;
  case IDNO: return 0;
  default: return -1;
  }
}
#else
# define C_HAS_MESSAGE_BOX 0
static int
C_confirmation_dialog(char *msg, char *caption, int def, int abort) { return -1; }
#endif
<#

;; Note: for Mac OS X, "CFUserNotificationDisplayAlert" could be used,
;;       unless that requires linking any libraries. This would also
;;       be useful for runtime error messages.

(define-foreign-variable C_HAS_MESSAGE_BOX bool)

(define yes-or-no?
  (let ((dialog (foreign-lambda int "C_confirmation_dialog" c-string c-string int bool)))
    (lambda (str #!key default title (abort reset))
      (let ((gui (and C_HAS_MESSAGE_BOX (not (##sys#fudge 4))))) ; C_gui_mode
	(define (get-input)
	  (if gui
	      (let ((r (dialog 
			str
			(or title "CHICKEN Runtime")
			(cond ((not default) 3)
			      ((string-ci=? default "yes") 0)
			      ((string-ci=? default "no") 1)
			      (else 2))
			abort)))
		(case r
		  ((0) "no")
		  ((1) "yes")
		  (else "abort")))
	      (string-trim-both (read-line))))
	(let loop ()
	  (unless gui
	    (printf "~%~A (yes/no~a) " str (if abort "/abort" ""))
	    (when default (printf "[~A] " default))
	    (flush-output))
	  (let ((ln (get-input)))
	    (cond ((eof-object? ln) (set! ln "abort"))
		  ((and default (string=? "" ln)) (set! ln default)) )
	    (cond ((string-ci=? "yes" ln) #t)
		  ((string-ci=? "no" ln) #f)
		  ((and abort (string-ci=? "abort" ln)) (abort))
		  (else
		   (if abort
		       (printf "~%Please enter \"yes\", \"no\" or \"abort\".~%")
		       (printf "~%Please enter \"yes\" or \"no\".~%"))
		   (loop) ) ) ) ) ) ) ) )
  
