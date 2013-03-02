;;;; setup-download.scm
;
; Copyright (c) 2008-2013, The Chicken Team
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


(require-library extras irregex posix utils setup-api srfi-1 data-structures tcp srfi-13
		 files)


(module setup-download (retrieve-extension
			locate-egg/local
			locate-egg/svn
			locate-egg/http
			gather-egg-information
			list-extensions
			list-extension-versions
			temporary-directory)

  (import scheme chicken foreign)
  (import extras irregex posix utils srfi-1 data-structures tcp srfi-13 srfi-14 files
	  setup-api)

  (define-constant +default-tcp-connect-timeout+ 30000) ; 30 seconds
  (define-constant +default-tcp-read/write-timeout+ 30000) ; 30 seconds

  (define-constant +url-regex+ "(http://)?([^/:]+)(:([^:/]+))?(/.*)?")

  (tcp-connect-timeout +default-tcp-connect-timeout+)
  (tcp-read-timeout +default-tcp-read/write-timeout+)
  (tcp-write-timeout +default-tcp-read/write-timeout+)

  (define *quiet* #f)
  (define *chicken-install-user-agent* (conc "chicken-install " (chicken-version)))
  (define *trunk* #f)
  (define *mode* 'default)
  (define *windows-shell* (foreign-value "C_WINDOWS_SHELL" bool))

  (define (d fstr . args)
    (let ([port (if *quiet* (current-error-port) (current-output-port))])
      (apply fprintf port fstr args)
      (flush-output port) ) )

  (define temporary-directory (make-parameter #f))

  (define (get-temporary-directory)
    (or (temporary-directory)
	(let ([dir (create-temporary-directory)])
	  (temporary-directory dir)
	  dir ) ) )

  (define (existing-version egg version vs)
    (if version
        (if (member version vs)
            version
            (error "version not found" egg version) )
        (let ([vs (sort vs version>=?)])
          (and (pair? vs)
               (car vs) ) ) ) )

  (define (when-no-such-version-warning egg version)
    (when version (warning "extension has no such version - using default" egg version)) )

  (define (list-eggs/local dir)
    (string-concatenate (map (cut string-append <> "\n") (directory dir))) )

  (define (list-egg-versions/local name dir)
    (let ((eggdir (make-pathname dir (string-append name "/tags"))))
      (cond ((directory-exists? eggdir)
	     (string-concatenate
	      (map (cut string-append <> "\n") (directory eggdir))))
	    (else "unknown\n"))))

  (define (locate-egg/local egg dir #!optional version destination clean)
    (let* ((eggdir (make-pathname dir egg))
	   (tagdir (make-pathname eggdir "tags"))
           (tagver (and (not *trunk*)
			(file-exists? tagdir) (directory? tagdir)
                        (existing-version egg version (directory tagdir)) ) )
	   (dest (and destination (make-pathname destination egg))))
      (let-values (((src ver)
		     (if tagver
			 (values (make-pathname tagdir tagver) tagver)
			 (let ((trunkdir (make-pathname eggdir "trunk")))
			   (when-no-such-version-warning egg version)
			   (if (and (file-exists? trunkdir) (directory? trunkdir))
			       (values trunkdir "trunk")
			       (values eggdir "") ) ) ) ) )
	(cond (dest
	       (create-directory dest)
	       (let ((qdest (qs (normalize-pathname dest)))
		     (qsrc (qs (normalize-pathname src)))
		     (cmd (if *windows-shell*
			      (sprintf "xcopy ~a ~a" src dest)
			      (sprintf "cp -r ~a/* ~a" src dest))))
		 (d "  ~a~%" cmd)
		 (if (zero? (system cmd))
		     (values dest ver)
		     (values #f ""))))
	      (else
	       ;; remove *.so files in toplevel dir, just for being careful
	       (when clean
		 (let ((sos (filter (cut string-suffix? ".so" <>) (directory src))))
		   (for-each
		    (lambda (f)
		      (d " deleting stale file `~a' from local build directory~%" f)
		      (delete-file* f))
		    sos)))
	       (values src ver))))))

  (define (gather-egg-information dir)	; used by salmonella (among others)
    (let ((ls (directory dir)))
      (filter-map
       (lambda (egg)
	 (let-values (((loc version) (locate-egg/local egg dir)))
	   (let ((meta (make-pathname loc egg "meta")))
	     (and (file-exists? meta)
		  (call/cc
		   (lambda (return)
		     (cons (string->symbol egg) 
			   (cons (list 'version version)
				 (handle-exceptions ex
				     (begin
				       (warning 
					"extension has syntactically invalid .meta file" 
					egg)
				       (return #f))
				   (with-input-from-file meta read))))))))))
       ls)))

  (define (make-svn-ls-cmd uarg parg pnam #!key recursive?)
    (conc "svn ls " uarg #\space parg (if recursive? " -R " " ") (qs pnam)) )

  (define (make-svn-export-cmd uarg parg dir tmpdir)
    (conc "svn export " uarg #\space parg #\space #\" dir #\" #\space #\" tmpdir #\"
          (if *quiet* " 1>&2" "")) )

  (define (list-eggs/svn repo #!optional username password)
    (let ([uarg (if username (string-append "--username='" username "'") "")]
          [parg (if password (string-append "--password='" password "'") "")])
      (let ([cmd (make-svn-ls-cmd uarg parg repo)])
        (d "listing extension directory ...~%  ~a~%" cmd)
        (string-concatenate
         (map (lambda (s) (string-append (string-chomp s "/") "\n"))
              (with-input-from-pipe cmd read-lines))) ) ) )

  (define (list-egg-versions/svn name repo #!optional username password)
    (let* ((uarg (if username (string-append "--username='" username "'") ""))
	   (parg (if password (string-append "--password='" password "'") ""))
	   (cmd (make-svn-ls-cmd uarg parg (make-pathname repo (string-append name "/tags"))))
	   (input (with-input-from-pipe cmd read-lines)))
      (if (null? input)
	  "unknown\n"
	  (string-concatenate
	   (map (lambda (s) (string-append (string-chomp s "/") "\n"))
		(with-input-from-pipe cmd read-lines))) ) ))

  (define (locate-egg/svn egg repo #!optional version destination username password)
    (let* ([uarg (if username (string-append "--username='" username "'") "")]
	   [parg (if password (string-append "--password='" password "'") "")]
	   [cmd (make-svn-ls-cmd uarg parg (make-pathname repo egg) recursive?: #t)])
      (d "checking available versions ...~%  ~a~%" cmd)
      (let* ([files (with-input-from-pipe cmd read-lines)]
	     [tagver (existing-version
	              egg version
	              (filter-map
                       (lambda (f) 
			 (and-let* ((m (irregex-search "^tags/([^/]+)/" f))) 
			   (irregex-match-substring m 1)))
                       files))])
	(let-values ([(filedir ver)
	              (if tagver
                          (values (string-append "tags/" tagver) tagver)
                          (begin
                            (when-no-such-version-warning egg version)
                            (if (member "trunk/" files)
                                (values "trunk" "trunk")
                                (values "" "") ) ) ) ] )
          (let* ((tmpdir (make-pathname (or destination (get-temporary-directory)) egg))
                 (cmd (make-svn-export-cmd 
		       uarg parg
		       (conc 
			repo #\/ egg #\/
			(if (eq? *mode* 'meta)
			    (metafile filedir egg)
			    filedir))
		       (if (eq? *mode* 'meta)
			   (begin
			     (create-directory tmpdir)
			     (metafile tmpdir egg))
			   tmpdir))))
	    (d "  ~a~%" cmd)
	    (if (zero? (system cmd))
                (values tmpdir ver)
	        (values #f "") ) ) ) ) ) )

  (define (metafile dir egg)
    (conc dir #\/ egg ".meta"))

  (define (deconstruct-url url)
    (let ([m (irregex-match +url-regex+ url)])
      (values
       (if m (irregex-match-substring m 2) url)
       (if (and m (irregex-match-substring m 3))
	   (let ((port (irregex-match-substring m 4)))
	     (or (string->number port)
		 (error "not a valid port" port)))
	   80)
       (or (and m (irregex-match-substring m 5))
           "/"))))

  (define (locate-egg/http egg url #!optional version destination tests
			   proxy-host proxy-port proxy-user-pass)
    (let ([tmpdir (or destination (get-temporary-directory))])
      (let-values ([(host port locn) (deconstruct-url url)])
	(let ([locn (string-append
		     locn
		     "?name=" egg
		     (if version (string-append "&version=" version) "")
		     "&mode=" (->string *mode*)
		     (if tests "&tests=yes" ""))]
	      [eggdir (make-pathname tmpdir egg) ] )
	  (unless (file-exists? eggdir) (create-directory eggdir))
	  (let ((fversion
		 (http-fetch host port locn eggdir proxy-host proxy-port proxy-user-pass)))
	    ;; If we get here then version of egg exists
	    (values eggdir (or fversion version "")) ) ) ) ))

  (define (network-failure msg . args)
    (signal
     (make-composite-condition
      (make-property-condition
       'exn
       'message "invalid response from server"
       'arguments args)
      (make-property-condition 'http-fetch))) )

  (define (make-HTTP-GET/1.1 location user-agent host
                             #!key
                             (port 80)
                             (connection "close")
                             (accept "*")
                             (content-length 0)
			     proxy-host proxy-port proxy-user-pass)
    (conc
     "GET " 
     (if proxy-host 
	 (string-append "http://" host location)
	 location)
     " HTTP/1.1" "\r\n"
     "Connection: " connection "\r\n"
     "User-Agent: " user-agent "\r\n"
     "Accept: " accept "\r\n"
     "Host: " host #\: port "\r\n"
     (if proxy-user-pass
         (string-append "Proxy-Authorization: Basic " proxy-user-pass "\r\n")
         "")
     "Content-length: " content-length "\r\n"
     "\r\n") )

  (define (match-http-response rsp)
    (and (string? rsp)
         (irregex-match "HTTP/[0-9.]+\\s+([0-9]+)\\s+.*" rsp)) )

  (define (response-match-code? mrsp code)
    (and mrsp (string=? (number->string code) (irregex-match-substring mrsp 1))) )

  (define (match-chunked-transfer-encoding ln)
    (irregex-match "[Tt]ransfer-[Ee]ncoding:\\s*chunked.*" ln) )

  (define (http-connect host port locn proxy-host proxy-port proxy-user-pass)
    (d "connecting to host ~s, port ~a ~a...~%" host port
       (if proxy-host
	   (sprintf "(via ~a:~a) " proxy-host proxy-port)
	   ""))
    (let-values (((in out) (tcp-connect (or proxy-host host) (or proxy-port port))))
      (d "requesting ~s ...~%" locn)
      (display
       (make-HTTP-GET/1.1 locn *chicken-install-user-agent* host port: port accept: "*/*"
			  proxy-host: proxy-host proxy-port: proxy-port)
       out)
      (flush-output out)
      (d "reading response ...~%")
      (let ([chunked #f])
	(let* ([h1 (read-line in)]
	       [response-match (match-http-response h1)])
	  (d "~a~%" h1)
	  ;;XXX handle redirects here
	  (if (response-match-code? response-match 407)
	      (let-values (((inpx outpx) (tcp-connect proxy-host proxy-port)))
		(set! in inpx) (set! out outpx)
		(display
		 (make-HTTP-GET/1.1 
		  locn *chicken-install-user-agent* host port: port accept: "*/*"
		  proxy-host: proxy-host proxy-port: proxy-port 
		  proxy-user-pass: proxy-user-pass)
		 out))
	      (unless (response-match-code? response-match 200)
		(network-failure "invalid response from server" h1)))
	  (let loop ()
	    (let ([ln (read-line in)])
	      (unless (string-null? ln)
		(when (match-chunked-transfer-encoding ln) (set! chunked #t))
		(d "~a~%" ln)
		(loop) ) ) ) )
	(when chunked
	  (d "reading chunks ")
	  (let ([data (read-chunks in)])
	    (close-input-port in)
	    (set! in (open-input-string data))) ) )
      (values in out)))

  (define (http-retrieve-files in out dest)
    (d "reading files ...~%")
    (let ((version #f))
      (define (skip)
	(let ((ln (read-line in)))
	  (cond ((or (eof-object? ln)
		     (irregex-match " *#!eof *" ln))
		 (open-input-string ""))
		((irregex-match " *#\\|[- ]*([^- ]*) *\\|#.*" ln) =>
		 (lambda (m)
		   (let ((v (irregex-match-substring m 1)))
		     (cond ((or (string=? "" v) (string=? "#f" v)))
			   ((and version (not (string=? v version)))
			    (warning "files-versions are not identical" ln version)
			    (set! version #f))
			   (else
			    (set! version v)))
		     (open-input-string ln))))
		((string-every char-set:whitespace ln)
		 (skip))
		(else
		 (error "unrecognized file-information - possibly corrupt transmission" 
			ln)))))
      (let get-files ((files '()))
	(let ((ins (skip)))
	  (let ((name (read ins)))
	    (cond ((and (pair? name) (eq? 'error (car name)))
		   (throw-server-error (cadr name) (cddr name)))
		  ((or (eof-object? name) (not name))
		   (close-input-port in)
		   (close-output-port out)
		   version)
		  ((not (string? name))
		   (error "invalid file name - possibly corrupt transmission" name) )
		  ((string-suffix? "/" name)
		   (d "  ~a~%" name)
		   (create-directory (make-pathname dest name))
		   (get-files files) )
		  (else
		   (d "  ~a~%" name)
		   (let* ((size (read ins))
			  (data (read-string size in)) )
		     (with-output-to-file (make-pathname dest name) (cut display data) #:binary ) )
		   (get-files (cons name files)) ) ) ) ) )))

  (define (http-fetch host port locn dest proxy-host proxy-port proxy-user-pass)
    (let-values (((in out)
		  (http-connect host port locn proxy-host proxy-port proxy-user-pass)))
      (http-retrieve-files in out dest)))

  (define (list-eggs/http location proxy-host proxy-port proxy-user-pass)
    (let-values ([(host port locn) (deconstruct-url location)])
      (let-values (((in out) 
		    (http-connect 
		     host port
		     (string-append locn "?list=1")
		     proxy-host proxy-port proxy-user-pass)))
	(let ((ls (read-all in)))
	  (close-input-port in)
	  (close-output-port out)
	  ls))))

  (define (throw-server-error msg args)
    (abort
     (make-composite-condition
      (make-property-condition
       'exn
       'message (string-append "[Server] " msg)
       'arguments args)
      (make-property-condition 'setup-download-error))))

  (define (read-chunks in)
    (let get-chunks ([data '()])
      (let* ((szln (read-line in))
	     ;;XXX workaround for "read-line" dropping the "\n" in certain situations
	     ;;    (#568)
	     (size (string->number (string-chomp szln "\r") 16)))
	(cond ((not size)
	       (error "invalid response from server - please try again"))
	      ((zero? size)
	       (d "~%")
	       (string-concatenate-reverse data))
	      (else
	       (let ([chunk (read-string size in)])
		 (d ".")
		 (read-line in)
		 (get-chunks (cons chunk data)) ) ) ) ) ))

  (define slashes (char-set #\\ #\/))

  (define (valid-extension-name? name)
    (and (not (member name '("" ".." ".")))
	 (not (string-index name slashes))))

  (define (check-egg-name name)
    (unless (valid-extension-name? name)
      (error "invalid extension name" name)))

  (define (retrieve-extension name transport location
                              #!key version quiet destination username password tests
			      proxy-host proxy-port proxy-user-pass
			      trunk (mode 'default) clean)
    (check-egg-name name)
    (fluid-let ((*quiet* quiet)
		(*trunk* trunk)
		(*mode* mode))
      (case transport
	((local)
	 (locate-egg/local name location version destination clean) )
	((svn)
	 (locate-egg/svn name location version destination username password) )
	((http)
	 (locate-egg/http name location version destination tests proxy-host proxy-port proxy-user-pass) )
	(else
	 (error "cannot retrieve extension - unsupported transport" transport) ) ) ) )

  (define (list-extensions transport location #!key quiet username password
			   proxy-host proxy-port proxy-user-pass)
    (fluid-let ((*quiet* quiet))
      (case transport
	((local)
	 (list-eggs/local location) )
	((svn)
	 (list-eggs/svn location username password) )
	((http)
	 (list-eggs/http location proxy-host proxy-port proxy-user-pass))
	(else
	 (error "cannot list extensions - unsupported transport" transport) ) ) ) )

  (define (list-extension-versions name transport location #!key quiet username password)
    (check-egg-name name)
    (fluid-let ((*quiet* quiet))
      (case transport
	((local)
	 (list-egg-versions/local name location) )
	((svn)
	 (list-egg-versions/svn name location username password) )
	(else
	 (error "cannot list extensions - unsupported transport" transport) ) ) ) )

) ;module setup-download
