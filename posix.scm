;;;; posix.scm - Platform-specific routines
;
; Copyright (c) 2008-2017, The CHICKEN Team
; Copyright (c) 2000-2007, Felix L. Winkelmann
; All rights reserved.
;
; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions are
; met:
;
;   Redistributions of source code must retain the above copyright
;   notice, this list of conditions and the following disclaimer.
;
;   Redistributions in binary form must reproduce the above copyright
;   notice, this list of conditions and the following disclaimer in the
;   documentation and/or other materials provided with the distribution.
;
;   Neither the name of the author nor the names of its contributors may
;   be used to endorse or promote products derived from this software
;   without specific prior written permission.
;
; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
; HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
; BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
; OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR
; TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
; USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
; DAMAGE.


(declare
  (unit posix)
  (uses scheduler irregex pathname extras port lolevel)
  (disable-interrupts)
  (not inline ##sys#interrupt-hook ##sys#user-interrupt-hook))

(cond-expand
  (platform-unix
   (include "posixunix.scm"))
  (platform-windows
   (include "posixwin.scm")))

(module chicken.errno *
(import scheme chicken)
(export errno)
(define errno/2big _e2big)
(define errno/acces _eacces)
(define errno/again _eagain)
(define errno/badf _ebadf)
(define errno/busy _ebusy)
(define errno/child _echild)
(define errno/deadlk _edeadlk)
(define errno/dom _edom)
(define errno/exist _eexist)
(define errno/fault _efault)
(define errno/fbig _efbig)
(define errno/ilseq _eilseq)
(define errno/intr _eintr)
(define errno/inval _einval)
(define errno/io _eio)
(define errno/isdir _eisdir)
(define errno/mfile _emfile)
(define errno/mlink _emlink)
(define errno/nametoolong _enametoolong)
(define errno/nfile _enfile)
(define errno/nodev _enodev)
(define errno/noent _enoent)
(define errno/noexec _enoexec)
(define errno/nolck _enolck)
(define errno/nomem _enomem)
(define errno/nospc _enospc)
(define errno/nosys _enosys)
(define errno/notdir _enotdir)
(define errno/notempty _enotempty)
(define errno/notty _enotty)
(define errno/nxio _enxio)
(define errno/perm _eperm)
(define errno/pipe _epipe)
(define errno/range _erange)
(define errno/rofs _erofs)
(define errno/spipe _espipe)
(define errno/srch _esrch)
(define errno/wouldblock _ewouldblock)
(define errno/xdev _exdev))

(module chicken.file.posix
  (duplicate-fileno fcntl/dupfd fcntl/getfd fcntl/getfl fcntl/setfd
   fcntl/setfl file-access-time file-change-time file-modification-time
   file-close file-control file-creation-mode file-link file-lock
   file-lock/blocking file-mkstemp file-open file-owner file-permissions
   file-position file-read file-select file-size file-stat
   file-test-lock file-truncate file-unlock file-write fileno/stderr
   fileno/stdin fileno/stdout open-input-file* open-output-file*
   open/append open/binary open/creat open/excl open/fsync open/noctty
   open/nonblock open/rdonly open/rdwr open/read open/sync open/text
   open/trunc open/write open/wronly perm/irgrp perm/iroth perm/irusr
   perm/irwxg perm/irwxo perm/irwxu perm/isgid perm/isuid perm/isvtx
   perm/iwgrp perm/iwoth perm/iwusr perm/ixgrp perm/ixoth perm/ixusr
   port->fileno seek/cur seek/end seek/set set-file-permissions!
   set-file-position! set-file-times!)
(import chicken chicken.posix))

(module chicken.time.posix
  (seconds->utc-time utc-time->seconds seconds->local-time
   seconds->string local-time->seconds string->time time->string
   local-timezone-abbreviation)
(import chicken chicken.posix))

(module chicken.process
  (qs system system* process-execute process-fork process-run
   process-signal process-wait call-with-input-pipe
   call-with-output-pipe close-input-pipe close-output-pipe create-pipe
   open-input-pipe open-output-pipe with-input-from-pipe
   with-output-to-pipe process process* pipe/buf process-group-id
   create-session)

(import chicken scheme chicken.posix chicken.platform)


;;; Like `system', but bombs on nonzero return code:

(define (system* str)
  (let ((n (system str)))
    (unless (zero? n)
      (##sys#error "shell invocation failed with non-zero return status" str n))))


;;; Quote string for shell:

(define (qs str #!optional (platform (build-platform)))
  (let* ((delim (if (eq? platform 'mingw32) #\" #\'))
	 (escaped (if (eq? platform 'mingw32) "\"\"" "'\\''"))
	 (escaped-parts
	  (map (lambda (c)
		 (cond
		   ((char=? c delim) escaped)
		   ((char=? c #\nul)
		    (error 'qs "NUL character can not be represented in shell string" str))
		   (else (string c))))
	       (string->list str))))
    (string-append
     (string delim)
     (apply string-append escaped-parts)
     (string delim)))))

(module chicken.process.signal
  (set-signal-handler! set-signal-mask! signal-handler signal-mask
   signal-mask! signal-masked? signal-unmask! signal/abrt signal/alrm
   signal/break signal/bus signal/chld signal/cont signal/fpe signal/hup
   signal/ill signal/int signal/io signal/kill signal/pipe signal/prof
   signal/quit signal/segv signal/stop signal/term signal/trap
   signal/tstp signal/urg signal/usr1 signal/usr2 signal/vtalrm
   signal/winch signal/xcpu signal/xfsz set-alarm!)
(import chicken chicken.posix))

(module chicken.process-context
  (command-line-arguments argv get-environment-variable
   get-environment-variables set-environment-variable!
   unset-environment-variable! emergency-exit exit on-exit
   executable-pathname program-name current-directory
   set-root-directory! current-effective-group-id
   current-effective-user-id current-group-id current-process-id
   current-user-id parent-process-id current-user-name
   current-effective-user-name user-information)
(import chicken chicken.posix))
