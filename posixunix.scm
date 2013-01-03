;;;; posixunix.scm - Miscellaneous file- and process-handling routines
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
  (unit posix)
  (uses scheduler irregex extras utils files ports)
  (disable-interrupts)
  (hide group-member _get-groups _ensure-groups posix-error ##sys#terminal-check)
  (not inline ##sys#interrupt-hook ##sys#user-interrupt-hook))


;; these are not available on Windows

(define-foreign-variable _stat_st_blksize unsigned-int "C_statbuf.st_blksize")
(define-foreign-variable _stat_st_blocks unsigned-int "C_statbuf.st_blocks")


;;; common code

(include "posix-common.scm")


(declare
  (foreign-declare #<<EOF
static C_TLS int C_wait_status;

#include <unistd.h>
#include <sys/time.h>
#include <sys/wait.h>
#include <sys/utsname.h>
#include <sys/ioctl.h>
#include <fcntl.h>
#include <dirent.h>
#include <pwd.h>
#include <utime.h>

#if defined(__sun__) && defined(__svr4__)
# include <sys/tty.h>
#endif

#ifdef HAVE_GRP_H
#include <grp.h>
#endif

#include <sys/mman.h>
#include <time.h>

#ifndef O_FSYNC
# define O_FSYNC O_SYNC
#endif

#ifndef PIPE_BUF
# ifdef __CYGWIN__
#  define PIPE_BUF       _POSIX_PIPE_BUF
# else
#  define PIPE_BUF 1024
# endif
#endif

#ifndef O_BINARY
# define O_BINARY        0
#endif
#ifndef O_TEXT
# define O_TEXT          0
#endif

#ifndef ARG_MAX
# define ARG_MAX 256
#endif

#ifndef MAP_FILE
# define MAP_FILE    0
#endif

#ifndef MAP_ANON
# define MAP_ANON    0
#endif

#if defined(HAVE_CRT_EXTERNS_H)
# include <crt_externs.h>
# define C_getenventry(i)       ((*_NSGetEnviron())[ i ])
#elif defined(C_MACOSX)
# define C_getenventry(i)       NULL
#else
extern char **environ;
# define C_getenventry(i)       (environ[ i ])
#endif

#ifndef ENV_MAX
# define ENV_MAX        1024
#endif

#ifndef FILENAME_MAX
# define FILENAME_MAX          1024
#endif

static C_TLS char *C_exec_args[ ARG_MAX ];
static C_TLS char *C_exec_env[ ENV_MAX ];
static C_TLS struct utsname C_utsname;
static C_TLS struct flock C_flock;
static C_TLS DIR *temphandle;
static C_TLS struct passwd *C_user;
#ifdef HAVE_GRP_H
static C_TLS struct group *C_group;
#else
static C_TLS struct {
  char *gr_name, gr_passwd;
  int gr_gid;
  char *gr_mem[ 1 ];
} C_group = { "", "", 0, { "" } };
#endif
static C_TLS int C_pipefds[ 2 ];
static C_TLS time_t C_secs;
static C_TLS struct tm C_tm;
static C_TLS fd_set C_fd_sets[ 2 ];
static C_TLS struct timeval C_timeval;
static C_TLS char C_hostbuf[ 256 ];
static C_TLS struct stat C_statbuf;

#define C_mkdir(str)        C_fix(mkdir(C_c_string(str), S_IRWXU | S_IRWXG | S_IRWXO))
#define C_fchdir(fd)        C_fix(fchdir(C_unfix(fd)))
#define C_chdir(str)        C_fix(chdir(C_c_string(str)))
#define C_rmdir(str)        C_fix(rmdir(C_c_string(str)))

#define C_opendir(x,h)      C_set_block_item(h, 0, (C_word) opendir(C_c_string(x)))
#define C_closedir(h)       (closedir((DIR *)C_block_item(h, 0)), C_SCHEME_UNDEFINED)
#define C_readdir(h,e)      C_set_block_item(e, 0, (C_word) readdir((DIR *)C_block_item(h, 0)))
#define C_foundfile(e,b)    (strcpy(C_c_string(b), ((struct dirent *) C_block_item(e, 0))->d_name), C_fix(strlen(((struct dirent *) C_block_item(e, 0))->d_name)))

#define open_binary_input_pipe(a, n, name)   C_mpointer(a, popen(C_c_string(name), "r"))
#define open_text_input_pipe(a, n, name)     open_binary_input_pipe(a, n, name)
#define open_binary_output_pipe(a, n, name)  C_mpointer(a, popen(C_c_string(name), "w"))
#define open_text_output_pipe(a, n, name)    open_binary_output_pipe(a, n, name)
#define close_pipe(p)                        C_fix(pclose(C_port_file(p)))

#define C_fork              fork
#define C_waitpid(id, o)    C_fix(waitpid(C_unfix(id), &C_wait_status, C_unfix(o)))
#define C_getppid           getppid
#define C_kill(id, s)       C_fix(kill(C_unfix(id), C_unfix(s)))
#define C_getuid            getuid
#define C_getgid            getgid
#define C_geteuid           geteuid
#define C_getegid           getegid
#define C_chown(fn, u, g)   C_fix(chown(C_data_pointer(fn), C_unfix(u), C_unfix(g)))
#define C_chmod(fn, m)      C_fix(chmod(C_data_pointer(fn), C_unfix(m)))
#define C_setuid(id)        C_fix(setuid(C_unfix(id)))
#define C_setgid(id)        C_fix(setgid(C_unfix(id)))
#define C_seteuid(id)       C_fix(seteuid(C_unfix(id)))
#define C_setegid(id)       C_fix(setegid(C_unfix(id)))
#define C_setsid(dummy)     C_fix(setsid())
#define C_setpgid(x, y)     C_fix(setpgid(C_unfix(x), C_unfix(y)))
#define C_getpgid(x)        C_fix(getpgid(C_unfix(x)))
#define C_symlink(o, n)     C_fix(symlink(C_data_pointer(o), C_data_pointer(n)))
#define C_do_readlink(f, b)    C_fix(readlink(C_data_pointer(f), C_data_pointer(b), FILENAME_MAX))
#define C_getpwnam(n)       C_mk_bool((C_user = getpwnam((char *)C_data_pointer(n))) != NULL)
#define C_getpwuid(u)       C_mk_bool((C_user = getpwuid(C_unfix(u))) != NULL)
#ifdef HAVE_GRP_H
#define C_getgrnam(n)       C_mk_bool((C_group = getgrnam((char *)C_data_pointer(n))) != NULL)
#define C_getgrgid(u)       C_mk_bool((C_group = getgrgid(C_unfix(u))) != NULL)
#else
#define C_getgrnam(n)       C_SCHEME_FALSE
#define C_getgrgid(n)       C_SCHEME_FALSE
#endif
#define C_pipe(d)           C_fix(pipe(C_pipefds))
#define C_truncate(f, n)    C_fix(truncate((char *)C_data_pointer(f), C_num_to_int(n)))
#define C_ftruncate(f, n)   C_fix(ftruncate(C_unfix(f), C_num_to_int(n)))
#define C_uname             C_fix(uname(&C_utsname))
#define C_alarm             alarm
#define C_setvbuf(p, m, s)  C_fix(setvbuf(C_port_file(p), NULL, C_unfix(m), C_unfix(s)))
#define C_test_access(fn, m)     C_fix(access((char *)C_data_pointer(fn), C_unfix(m)))
#define C_close(fd)         C_fix(close(C_unfix(fd)))
#define C_sleep             sleep
#define C_umask(m)          C_fix(umask(C_unfix(m)))

#define C_lstat(fn)         C_fix(lstat((char *)C_data_pointer(fn), &C_statbuf))

#ifdef C_GNU_ENV
# define C_unsetenv(s)      (unsetenv((char *)C_data_pointer(s)), C_SCHEME_TRUE)
# define C_setenv(x, y)     C_fix(setenv((char *)C_data_pointer(x), (char *)C_data_pointer(y), 1))
#else
# define C_unsetenv(s)      C_fix(putenv((char *)C_data_pointer(s)))
static C_word C_fcall C_setenv(C_word x, C_word y) {
  char *sx = C_data_pointer(x),
       *sy = C_data_pointer(y);
  int n1 = C_strlen(sx), n2 = C_strlen(sy);
  char *buf = (char *)C_malloc(n1 + n2 + 2);
  if(buf == NULL) return(C_fix(0));
  else {
    C_strcpy(buf, sx);
    buf[ n1 ] = '=';
    C_strcpy(buf + n1 + 1, sy);
    return(C_fix(putenv(buf)));
  }
}
#endif

static void C_fcall C_set_arg_string(char **where, int i, char *a, int len) {
  char *ptr;
  if(a != NULL) {
    ptr = (char *)C_malloc(len + 1);
    C_memcpy(ptr, a, len);
    ptr[ len ] = '\0';
    /* Can't barf() here, so the NUL byte check happens in Scheme */
  }
  else ptr = NULL;
  where[ i ] = ptr;
}

static void C_fcall C_free_arg_string(char **where) {
  while((*where) != NULL) C_free(*(where++));
}

static void C_set_timeval(C_word num, struct timeval *tm)
{
  if((num & C_FIXNUM_BIT) != 0) {
    tm->tv_sec = C_unfix(num);
    tm->tv_usec = 0;
  }
  else {
    double i;
    tm->tv_usec = (int)(modf(C_flonum_magnitude(num), &i) * 1000000);
    tm->tv_sec = (int)i;
  }
}

#define C_set_exec_arg(i, a, len)	C_set_arg_string(C_exec_args, i, a, len)
#define C_free_exec_args()		C_free_arg_string(C_exec_args)
#define C_set_exec_env(i, a, len)	C_set_arg_string(C_exec_env, i, a, len)
#define C_free_exec_env()		C_free_arg_string(C_exec_env)

#define C_execvp(f)         C_fix(execvp(C_data_pointer(f), C_exec_args))
#define C_execve(f)         C_fix(execve(C_data_pointer(f), C_exec_args, C_exec_env))

#if defined(__FreeBSD__) || defined(C_MACOSX) || defined(__NetBSD__) || defined(__OpenBSD__) || defined(__sgi__) || defined(sgi) || defined(__DragonFly__) || defined(__SUNPRO_C)
static C_TLS int C_uw;
# define C_WIFEXITED(n)      (C_uw = C_unfix(n), C_mk_bool(WIFEXITED(C_uw)))
# define C_WIFSIGNALED(n)    (C_uw = C_unfix(n), C_mk_bool(WIFSIGNALED(C_uw)))
# define C_WIFSTOPPED(n)     (C_uw = C_unfix(n), C_mk_bool(WIFSTOPPED(C_uw)))
# define C_WEXITSTATUS(n)    (C_uw = C_unfix(n), C_fix(WEXITSTATUS(C_uw)))
# define C_WTERMSIG(n)       (C_uw = C_unfix(n), C_fix(WTERMSIG(C_uw)))
# define C_WSTOPSIG(n)       (C_uw = C_unfix(n), C_fix(WSTOPSIG(C_uw)))
#else
# define C_WIFEXITED(n)      C_mk_bool(WIFEXITED(C_unfix(n)))
# define C_WIFSIGNALED(n)    C_mk_bool(WIFSIGNALED(C_unfix(n)))
# define C_WIFSTOPPED(n)     C_mk_bool(WIFSTOPPED(C_unfix(n)))
# define C_WEXITSTATUS(n)    C_fix(WEXITSTATUS(C_unfix(n)))
# define C_WTERMSIG(n)       C_fix(WTERMSIG(C_unfix(n)))
# define C_WSTOPSIG(n)       C_fix(WSTOPSIG(C_unfix(n)))
#endif

#ifdef __CYGWIN__
# define C_mkfifo(fn, m)    C_fix(-1);
#else
# define C_mkfifo(fn, m)    C_fix(mkfifo((char *)C_data_pointer(fn), C_unfix(m)))
#endif

#define C_flock_setup(t, s, n) (C_flock.l_type = C_unfix(t), C_flock.l_start = C_num_to_int(s), C_flock.l_whence = SEEK_SET, C_flock.l_len = C_num_to_int(n), C_SCHEME_UNDEFINED)
#define C_flock_test(p)     (fcntl(fileno(C_port_file(p)), F_GETLK, &C_flock) >= 0 ? (C_flock.l_type == F_UNLCK ? C_fix(0) : C_fix(C_flock.l_pid)) : C_SCHEME_FALSE)
#define C_flock_lock(p)     C_fix(fcntl(fileno(C_port_file(p)), F_SETLK, &C_flock))
#define C_flock_lockw(p)    C_fix(fcntl(fileno(C_port_file(p)), F_SETLKW, &C_flock))

static C_TLS sigset_t C_sigset;
#define C_sigemptyset(d)    (sigemptyset(&C_sigset), C_SCHEME_UNDEFINED)
#define C_sigaddset(s)      (sigaddset(&C_sigset, C_unfix(s)), C_SCHEME_UNDEFINED)
#define C_sigdelset(s)      (sigdelset(&C_sigset, C_unfix(s)), C_SCHEME_UNDEFINED)
#define C_sigismember(s)    C_mk_bool(sigismember(&C_sigset, C_unfix(s)))
#define C_sigprocmask_set(d)        C_fix(sigprocmask(SIG_SETMASK, &C_sigset, NULL))
#define C_sigprocmask_block(d)      C_fix(sigprocmask(SIG_BLOCK, &C_sigset, NULL))
#define C_sigprocmask_unblock(d)    C_fix(sigprocmask(SIG_UNBLOCK, &C_sigset, NULL))

#define C_open(fn, fl, m)   C_fix(open(C_c_string(fn), C_unfix(fl), C_unfix(m)))
#define C_read(fd, b, n)    C_fix(read(C_unfix(fd), C_data_pointer(b), C_unfix(n)))
#define C_write(fd, b, n)   C_fix(write(C_unfix(fd), C_data_pointer(b), C_unfix(n)))
#define C_mkstemp(t)        C_fix(mkstemp(C_c_string(t)))

/* It is assumed that 'int' is-a 'long' */
#define C_ftell(p)          C_fix(ftell(C_port_file(p)))
#define C_fseek(p, n, w)    C_mk_nbool(fseek(C_port_file(p), C_num_to_int(n), C_unfix(w)))
#define C_lseek(fd, o, w)     C_fix(lseek(C_unfix(fd), C_unfix(o), C_unfix(w)))

#define C_zero_fd_set(i)      FD_ZERO(&C_fd_sets[ i ])
#define C_set_fd_set(i, fd)   FD_SET(fd, &C_fd_sets[ i ])
#define C_test_fd_set(i, fd)  FD_ISSET(fd, &C_fd_sets[ i ])
#define C_C_select(m)         C_fix(select(C_unfix(m), &C_fd_sets[ 0 ], &C_fd_sets[ 1 ], NULL, NULL))
#define C_C_select_t(m, t)    (C_set_timeval(t, &C_timeval), \
			       C_fix(select(C_unfix(m), &C_fd_sets[ 0 ], &C_fd_sets[ 1 ], NULL, &C_timeval)))

#define C_ctime(n)          (C_secs = (n), ctime(&C_secs))

#if defined(__SVR4) || defined(C_MACOSX)
/* Seen here: http://lists.samba.org/archive/samba-technical/2002-November/025571.html */

static time_t C_timegm(struct tm *t)
{
  time_t tl, tb;
  struct tm *tg;

  tl = mktime (t);
  if (tl == -1)
    {
      t->tm_hour--;
      tl = mktime (t);
      if (tl == -1)
        return -1; /* can't deal with output from strptime */
      tl += 3600;
    }
  tg = gmtime (&tl);
  tg->tm_isdst = 0;
  tb = mktime (tg);
  if (tb == -1)
    {
      tg->tm_hour--;
      tb = mktime (tg);
      if (tb == -1)
        return -1; /* can't deal with output from gmtime */
      tb += 3600;
    }
  return (tl - (tb - tl));
}
#else
#define C_timegm timegm
#endif

#define cpy_tmvec_to_tmstc08(ptm, v) \
    (memset((ptm), 0, sizeof(struct tm)), \
    (ptm)->tm_sec = C_unfix(C_block_item((v), 0)), \
    (ptm)->tm_min = C_unfix(C_block_item((v), 1)), \
    (ptm)->tm_hour = C_unfix(C_block_item((v), 2)), \
    (ptm)->tm_mday = C_unfix(C_block_item((v), 3)), \
    (ptm)->tm_mon = C_unfix(C_block_item((v), 4)), \
    (ptm)->tm_year = C_unfix(C_block_item((v), 5)), \
    (ptm)->tm_wday = C_unfix(C_block_item((v), 6)), \
    (ptm)->tm_yday = C_unfix(C_block_item((v), 7)), \
    (ptm)->tm_isdst = (C_block_item((v), 8) != C_SCHEME_FALSE))

#define cpy_tmvec_to_tmstc9(ptm, v) \
    (((struct tm *)ptm)->tm_gmtoff = -C_unfix(C_block_item((v), 9)))

#define cpy_tmstc08_to_tmvec(v, ptm) \
    (C_set_block_item((v), 0, C_fix(((struct tm *)ptm)->tm_sec)), \
    C_set_block_item((v), 1, C_fix((ptm)->tm_min)), \
    C_set_block_item((v), 2, C_fix((ptm)->tm_hour)), \
    C_set_block_item((v), 3, C_fix((ptm)->tm_mday)), \
    C_set_block_item((v), 4, C_fix((ptm)->tm_mon)), \
    C_set_block_item((v), 5, C_fix((ptm)->tm_year)), \
    C_set_block_item((v), 6, C_fix((ptm)->tm_wday)), \
    C_set_block_item((v), 7, C_fix((ptm)->tm_yday)), \
    C_set_block_item((v), 8, ((ptm)->tm_isdst ? C_SCHEME_TRUE : C_SCHEME_FALSE)))

#define cpy_tmstc9_to_tmvec(v, ptm) \
    (C_set_block_item((v), 9, C_fix(-(ptm)->tm_gmtoff)))

#define C_tm_set_08(v)  cpy_tmvec_to_tmstc08( &C_tm, (v) )
#define C_tm_set_9(v)   cpy_tmvec_to_tmstc9( &C_tm, (v) )

#define C_tm_get_08(v)  cpy_tmstc08_to_tmvec( (v), &C_tm )
#define C_tm_get_9(v)   cpy_tmstc9_to_tmvec( (v), &C_tm )

#if !defined(C_GNU_ENV) || defined(__CYGWIN__) || defined(__uClinux__)

static struct tm *
C_tm_set( C_word v )
{
  C_tm_set_08( v );
  return &C_tm;
}

static C_word
C_tm_get( C_word v )
{
  C_tm_get_08( v );
  return v;
}

#else

static struct tm *
C_tm_set( C_word v )
{
  C_tm_set_08( v );
  C_tm_set_9( v );
  return &C_tm;
}

static C_word
C_tm_get( C_word v )
{
  C_tm_get_08( v );
  C_tm_get_9( v );
  return v;
}

#endif

#define C_asctime(v)    (asctime(C_tm_set(v)))
#define C_a_mktime(ptr, c, v)  C_flonum(ptr, mktime(C_tm_set(v)))
#define C_a_timegm(ptr, c, v)  C_flonum(ptr, C_timegm(C_tm_set(v)))

#define TIME_STRING_MAXLENGTH 255
static char C_time_string [TIME_STRING_MAXLENGTH + 1];
#undef TIME_STRING_MAXLENGTH

#ifdef __linux__
extern char *strptime(const char *s, const char *format, struct tm *tm);
extern pid_t getpgid(pid_t pid);
#endif

#define C_strptime(s, f, v) \
        (strptime(C_c_string(s), C_c_string(f), &C_tm) ? C_tm_get(v) : C_SCHEME_FALSE)

static gid_t *C_groups = NULL;

#define C_get_gid(n)      C_fix(C_groups[ C_unfix(n) ])
#define C_set_gid(n, id)  (C_groups[ C_unfix(n) ] = C_unfix(id), C_SCHEME_UNDEFINED)
#define C_set_groups(n)   C_fix(setgroups(C_unfix(n), C_groups))

#ifdef TIOCGWINSZ
static int get_tty_size(int p, int *rows, int *cols)
{
 struct winsize tty_size;
 int r;

 memset(&tty_size, 0, sizeof tty_size);

 r = ioctl(p, TIOCGWINSZ, &tty_size);
 if (r == 0) {
    *rows = tty_size.ws_row;
    *cols = tty_size.ws_col;
 }
 return r;
}
#else
static int get_tty_size(int p, int *rows, int *cols)
{
 *rows = *cols = 0;
 return -1;
}
#endif

static int set_file_mtime(char *filename, C_word tm)
{
  struct utimbuf tb;

  tb.actime = tb.modtime = C_num_to_int(tm);
  return utime(filename, &tb);
}

static C_word C_i_fifo_p(C_word name) 
{
  struct stat buf;
  int res;

  res = stat(C_c_string(name), &buf);

  if(res != 0) {
#ifdef __CYGWIN__
    return C_SCHEME_FALSE;
#else
    if(errno == ENOENT) return C_fix(0);
    else return C_fix(res);
#endif
  }

  if((buf.st_mode & S_IFMT) == S_IFIFO) return C_SCHEME_TRUE;
  else return C_SCHEME_FALSE;
}

EOF
) )

;; Faster versions of common operations

(define ##sys#file-nonblocking!
  (foreign-lambda* bool ([int fd])
    "int val = fcntl(fd, F_GETFL, 0);"
    "if(val == -1) C_return(0);"
    "C_return(fcntl(fd, F_SETFL, val | O_NONBLOCK) != -1);" ) )

(define ##sys#file-select-one
  (foreign-lambda* int ([int fd])
    "fd_set in;"
    "struct timeval tm;"
    "FD_ZERO(&in);"
    "FD_SET(fd, &in);"
    "tm.tv_sec = tm.tv_usec = 0;"
    "if(select(fd + 1, &in, NULL, NULL, &tm) == -1) C_return(-1);"
    "else C_return(FD_ISSET(fd, &in) ? 1 : 0);" ) )


;;; Lo-level I/O:

(define-foreign-variable _pipe_buf int "PIPE_BUF")

(define pipe/buf _pipe_buf)

(define-foreign-variable _f_dupfd int "F_DUPFD")
(define-foreign-variable _f_getfd int "F_GETFD")
(define-foreign-variable _f_setfd int "F_SETFD")
(define-foreign-variable _f_getfl int "F_GETFL")
(define-foreign-variable _f_setfl int "F_SETFL")

(define fcntl/dupfd _f_dupfd)
(define fcntl/getfd _f_getfd)
(define fcntl/setfd _f_setfd)
(define fcntl/getfl _f_getfl)
(define fcntl/setfl _f_setfl)

(define-foreign-variable _o_rdonly int "O_RDONLY")
(define-foreign-variable _o_wronly int "O_WRONLY")
(define-foreign-variable _o_rdwr int "O_RDWR")
(define-foreign-variable _o_creat int "O_CREAT")
(define-foreign-variable _o_append int "O_APPEND")
(define-foreign-variable _o_excl int "O_EXCL")
(define-foreign-variable _o_noctty int "O_NOCTTY")
(define-foreign-variable _o_nonblock int "O_NONBLOCK")
(define-foreign-variable _o_trunc int "O_TRUNC")
(define-foreign-variable _o_fsync int "O_FSYNC")
(define-foreign-variable _o_binary int "O_BINARY")
(define-foreign-variable _o_text int "O_TEXT")

(define open/rdonly _o_rdonly)
(define open/wronly _o_wronly)
(define open/rdwr _o_rdwr)
(define open/read _o_rdonly)
(define open/write _o_wronly)
(define open/creat _o_creat)
(define open/append _o_append)
(define open/excl _o_excl)
(define open/noctty _o_noctty)
(define open/nonblock _o_nonblock)
(define open/trunc _o_trunc)
(define open/sync _o_fsync)
(define open/fsync _o_fsync)
(define open/binary _o_binary)
(define open/text _o_text)

(define-foreign-variable _s_irusr int "S_IRUSR")
(define-foreign-variable _s_iwusr int "S_IWUSR")
(define-foreign-variable _s_ixusr int "S_IXUSR")
(define-foreign-variable _s_irgrp int "S_IRGRP")
(define-foreign-variable _s_iwgrp int "S_IWGRP")
(define-foreign-variable _s_ixgrp int "S_IXGRP")
(define-foreign-variable _s_iroth int "S_IROTH")
(define-foreign-variable _s_iwoth int "S_IWOTH")
(define-foreign-variable _s_ixoth int "S_IXOTH")
(define-foreign-variable _s_irwxu int "S_IRWXU")
(define-foreign-variable _s_irwxg int "S_IRWXG")
(define-foreign-variable _s_irwxo int "S_IRWXO")
(define-foreign-variable _s_isuid int "S_ISUID")
(define-foreign-variable _s_isgid int "S_ISGID")
(define-foreign-variable _s_isvtx int "S_ISVTX")

(define perm/irusr _s_irusr)
(define perm/iwusr _s_iwusr)
(define perm/ixusr _s_ixusr)
(define perm/irgrp _s_irgrp)
(define perm/iwgrp _s_iwgrp)
(define perm/ixgrp _s_ixgrp)
(define perm/iroth _s_iroth)
(define perm/iwoth _s_iwoth)
(define perm/ixoth _s_ixoth)
(define perm/irwxu _s_irwxu)
(define perm/irwxg _s_irwxg)
(define perm/irwxo _s_irwxo)
(define perm/isvtx _s_isvtx)
(define perm/isuid _s_isuid)
(define perm/isgid _s_isgid)

(define file-control
  (let ([fcntl (foreign-lambda int fcntl int int long)])
    (lambda (fd cmd #!optional (arg 0))
      (##sys#check-exact fd 'file-control)
      (##sys#check-exact cmd 'file-control)
      (let ([res (fcntl fd cmd arg)])
        (if (fx= res -1)
            (posix-error #:file-error 'file-control "cannot control file" fd cmd)
            res ) ) ) ) )

(define file-open
  (let ([defmode (bitwise-ior _s_irwxu (bitwise-ior _s_irgrp _s_iroth))] )
    (lambda (filename flags . mode)
      (let ([mode (if (pair? mode) (car mode) defmode)])
        (##sys#check-string filename 'file-open)
        (##sys#check-exact flags 'file-open)
        (##sys#check-exact mode 'file-open)
        (let ([fd (##core#inline "C_open" (##sys#make-c-string (##sys#expand-home-path filename) 'file-open) flags mode)])
          (when (eq? -1 fd)
            (posix-error #:file-error 'file-open "cannot open file" filename flags mode) )
          fd) ) ) ) )

(define file-close
  (lambda (fd)
    (##sys#check-exact fd 'file-close)
    (when (fx< (##core#inline "C_close" fd) 0)
      (posix-error #:file-error 'file-close "cannot close file" fd) ) ) )

(define file-read
  (lambda (fd size . buffer)
    (##sys#check-exact fd 'file-read)
    (##sys#check-exact size 'file-read)
    (let ([buf (if (pair? buffer) (car buffer) (make-string size))])
      (unless (and (##core#inline "C_blockp" buf) (##core#inline "C_byteblockp" buf))
	(##sys#signal-hook #:type-error 'file-read "bad argument type - not a string or blob" buf) )
      (let ([n (##core#inline "C_read" fd buf size)])
	(when (eq? -1 n)
	  (posix-error #:file-error 'file-read "cannot read from file" fd size) )
	(list buf n) ) ) ) )

(define file-write
  (lambda (fd buffer . size)
    (##sys#check-exact fd 'file-write)
    (unless (and (##core#inline "C_blockp" buffer) (##core#inline "C_byteblockp" buffer))
      (##sys#signal-hook #:type-error 'file-write "bad argument type - not a string or blob" buffer) )
    (let ([size (if (pair? size) (car size) (##sys#size buffer))])
      (##sys#check-exact size 'file-write)
      (let ([n (##core#inline "C_write" fd buffer size)])
        (when (eq? -1 n)
          (posix-error #:file-error 'file-write "cannot write to file" fd size) )
        n) ) ) )

(define file-mkstemp
  (lambda (template)
    (##sys#check-string template 'file-mkstemp)
    (let* ([buf (##sys#make-c-string template 'file-mkstemp)]
	   [fd (##core#inline "C_mkstemp" buf)]
	   [path-length (##sys#size buf)])
      (when (eq? -1 fd)
	(posix-error #:file-error 'file-mkstemp "cannot create temporary file" template) )
      (values fd (##sys#substring buf 0 (fx- path-length 1) ) ) ) ) )


;;; I/O multiplexing:

(define file-select
  (let ([fd_zero (foreign-lambda void "C_zero_fd_set" int)]
        [fd_set (foreign-lambda void "C_set_fd_set" int int)]
        [fd_test (foreign-lambda bool "C_test_fd_set" int int)] )
    (lambda (fdsr fdsw . timeout)
      (let ([fdmax 0]
            [tm (if (pair? timeout) (car timeout) #f)] )
        (fd_zero 0)
        (fd_zero 1)
        (cond [(not fdsr)]
              [(fixnum? fdsr)
               (set! fdmax fdsr)
               (fd_set 0 fdsr) ]
              [else
               (##sys#check-list fdsr 'file-select)
               (for-each
                (lambda (fd)
                  (##sys#check-exact fd 'file-select)
                  (set! fdmax (##core#inline "C_i_fixnum_max" fdmax fd))
                  (fd_set 0 fd) )
                fdsr) ] )
        (cond [(not fdsw)]
              [(fixnum? fdsw)
               (set! fdmax fdsw)
               (fd_set 1 fdsw) ]
              [else
               (##sys#check-list fdsw 'file-select)
               (for-each
                (lambda (fd)
                  (##sys#check-exact fd 'file-select)
                  (set! fdmax (##core#inline "C_i_fixnum_max" fdmax fd))
                  (fd_set 1 fd) )
                fdsw) ] )
        (let ([n (cond [tm
                        (##sys#check-number tm 'file-select)
                        (##core#inline "C_C_select_t" (fx+ fdmax 1) tm) ]
                       [else (##core#inline "C_C_select" (fx+ fdmax 1))] ) ] )
          (cond [(fx< n 0)
                 (posix-error #:file-error 'file-select "failed" fdsr fdsw) ]
                [(fx= n 0) (values (if (pair? fdsr) '() #f) (if (pair? fdsw) '() #f))]
                [else
                 (values
                  (and fdsr
                       (if (fixnum? fdsr)
                           (fd_test 0 fdsr)
                           (let ([lstr '()])
                             (for-each (lambda (fd) (when (fd_test 0 fd) (set! lstr (cons fd lstr)))) fdsr)
                             lstr) ) )
                  (and fdsw
                       (if (fixnum? fdsw)
                           (fd_test 1 fdsw)
                           (let ([lstw '()])
                             (for-each (lambda (fd) (when (fd_test 1 fd) (set! lstw (cons fd lstw)))) fdsw)
                             lstw) ) ) ) ] ) ) ) ) ) )


;;; File attribute access:

(define-foreign-variable _seek_set int "SEEK_SET")
(define-foreign-variable _seek_cur int "SEEK_CUR")
(define-foreign-variable _seek_end int "SEEK_END")

(define seek/set _seek_set)
(define seek/end _seek_end)
(define seek/cur _seek_cur)

(define set-file-position!
  (lambda (port pos . whence)
    (let ((whence (if (pair? whence) (car whence) _seek_set)))
      (##sys#check-exact pos 'set-file-position!)
      (##sys#check-exact whence 'set-file-position!)
      (when (negative? pos)
	(##sys#signal-hook #:bounds-error 'set-file-position! "invalid negative port position" pos port))
      (unless (cond ((port? port)
		     (and (eq? (##sys#slot port 7) 'stream)
			  (##core#inline "C_fseek" port pos whence) ) )
		    ((fixnum? port)
		     (##core#inline "C_lseek" port pos whence))
		    (else
		     (##sys#signal-hook #:type-error 'set-file-position! "invalid file" port)) )
	(posix-error #:file-error 'set-file-position! "cannot set file position" port pos) ) ) ) )

(define file-position
  (getter-with-setter
   (lambda (port)
     (let ((pos (cond ((port? port)
		       (if (eq? (##sys#slot port 7) 'stream)
			   (##core#inline "C_ftell" port)
			   -1) )
		      ((fixnum? port)
		       (##core#inline "C_lseek" port 0 _seek_cur) )
		      (else
		       (##sys#signal-hook #:type-error 'file-position "invalid file" port)) ) ) )
       (when (< pos 0)
	 (posix-error #:file-error 'file-position "cannot retrieve file position of port" port) )
       pos) )
   set-file-position!		; doesn't accept WHENCE
   "(file-position port)"))


;;; Directory stuff:

(define-inline (*create-directory loc name)
  (unless (fx= 0 (##core#inline "C_mkdir" (##sys#make-c-string name loc)))
    (posix-error #:file-error loc "cannot create directory" name)) )

(define create-directory
  (lambda (name #!optional parents?)
    (##sys#check-string name 'create-directory)
    (let ((name (##sys#expand-home-path name)))
      (unless (or (fx= 0 (##sys#size name))
		  (file-exists? name))
	(if parents?
	    (let loop ((dir (let-values (((dir file ext) (decompose-pathname name)))
			      (if file (make-pathname dir file ext) dir))))
	      (when (and dir (not (directory? dir)))
		(loop (pathname-directory dir))
		(*create-directory 'create-directory dir)) )
	    (*create-directory 'create-directory name) ) )
      name)))

(define change-directory
  (lambda (name)
    (##sys#check-string name 'change-directory)
    (let ((sname (##sys#make-c-string (##sys#expand-home-path name) 'change-directory)))
      (unless (fx= 0 (##core#inline "C_chdir" sname))
	(posix-error #:file-error 'change-directory "cannot change current directory" name) )
      name)))

(define (change-directory* fd)
  (##sys#check-exact fd 'change-directory*) 
  (unless (fx= 0 (##core#inline "C_fchdir" fd)) 
    (posix-error #:file-error 'change-directory* "cannot change current directory" fd) )
  fd)


;;; Pipes:

(let ()
  (define (mode arg) (if (pair? arg) (##sys#slot arg 0) '###text))
  (define (badmode m) (##sys#error "illegal input/output mode specifier" m))
  (define (check loc cmd inp r)
    (if (##sys#null-pointer? r)
	(posix-error #:file-error loc "cannot open pipe" cmd)
	(let ([port (##sys#make-port inp ##sys#stream-port-class "(pipe)" 'stream)])
	  (##core#inline "C_set_file_ptr" port r)
	  port) ) )
  (set! open-input-pipe
    (lambda (cmd . m)
      (##sys#check-string cmd 'open-input-pipe)
      (let ([m (mode m)])
	(check
	 'open-input-pipe
	 cmd #t
	 (case m
	   ((#:text) (##core#inline_allocate ("open_text_input_pipe" 2) (##sys#make-c-string cmd 'open-input-pipe)))
	   ((#:binary) (##core#inline_allocate ("open_binary_input_pipe" 2) (##sys#make-c-string cmd 'open-input-pipe)))
	   (else (badmode m)) ) ) ) ) )
  (set! open-output-pipe
    (lambda (cmd . m)
      (##sys#check-string cmd 'open-output-pipe)
      (let ((m (mode m)))
	(check
	 'open-output-pipe
	 cmd #f
	 (case m
	   ((#:text) (##core#inline_allocate ("open_text_output_pipe" 2) (##sys#make-c-string cmd 'open-output-pipe)))
	   ((#:binary) (##core#inline_allocate ("open_binary_output_pipe" 2) (##sys#make-c-string cmd 'open-output-pipe)))
	   (else (badmode m)) ) ) ) ) )
  (set! close-input-pipe
    (lambda (port)
      (##sys#check-input-port port #t 'close-input-pipe)
      (let ((r (##core#inline "close_pipe" port)))
	(when (eq? -1 r)
	  (posix-error #:file-error 'close-input-pipe "error while closing pipe" port))
	r) ) )
  (set! close-output-pipe
    (lambda (port)
      (##sys#check-output-port port #t 'close-output-pipe)
      (let ((r (##core#inline "close_pipe" port)))
	(when (eq? -1 r) 
	  (posix-error #:file-error 'close-output-pipe "error while closing pipe" port))
	r) ) ))

(define call-with-input-pipe
  (lambda (cmd proc . mode)
    (let ([p (apply open-input-pipe cmd mode)])
      (##sys#call-with-values
       (lambda () (proc p))
       (lambda results
	 (close-input-pipe p)
	 (apply values results) ) ) ) ) )

(define call-with-output-pipe
  (lambda (cmd proc . mode)
    (let ([p (apply open-output-pipe cmd mode)])
      (##sys#call-with-values
       (lambda () (proc p))
       (lambda results
	 (close-output-pipe p)
	 (apply values results) ) ) ) ) )

(define with-input-from-pipe
  (lambda (cmd thunk . mode)
    (let ([p (apply open-input-pipe cmd mode)])
      (fluid-let ((##sys#standard-input p))
	(##sys#call-with-values thunk
				(lambda results
				  (close-input-pipe p)
				  (apply values results) ) ) ) ) ) )
(define with-output-to-pipe
  (lambda (cmd thunk . mode)
    (let ([p (apply open-output-pipe cmd mode)])
      (fluid-let ((##sys#standard-output p))
	(##sys#call-with-values thunk
				(lambda results
				  (close-output-pipe p)
				  (apply values results) ) ) ) ) ) )

(define-foreign-variable _pipefd0 int "C_pipefds[ 0 ]")
(define-foreign-variable _pipefd1 int "C_pipefds[ 1 ]")

(define create-pipe
  (lambda ()
    (when (fx< (##core#inline "C_pipe" #f) 0)
      (posix-error #:file-error 'create-pipe "cannot create pipe") )
    (values _pipefd0 _pipefd1) ) )


;;; Signal processing:

(define-foreign-variable _nsig int "NSIG")
(define-foreign-variable _sigterm int "SIGTERM")
(define-foreign-variable _sigkill int "SIGKILL")
(define-foreign-variable _sigint int "SIGINT")
(define-foreign-variable _sighup int "SIGHUP")
(define-foreign-variable _sigfpe int "SIGFPE")
(define-foreign-variable _sigill int "SIGILL")
(define-foreign-variable _sigsegv int "SIGSEGV")
(define-foreign-variable _sigabrt int "SIGABRT")
(define-foreign-variable _sigtrap int "SIGTRAP")
(define-foreign-variable _sigquit int "SIGQUIT")
(define-foreign-variable _sigalrm int "SIGALRM")
(define-foreign-variable _sigpipe int "SIGPIPE")
(define-foreign-variable _sigusr1 int "SIGUSR1")
(define-foreign-variable _sigusr2 int "SIGUSR2")
(define-foreign-variable _sigvtalrm int "SIGVTALRM")
(define-foreign-variable _sigprof int "SIGPROF")
(define-foreign-variable _sigio int "SIGIO")
(define-foreign-variable _sigurg int "SIGURG")
(define-foreign-variable _sigchld int "SIGCHLD")
(define-foreign-variable _sigcont int "SIGCONT")
(define-foreign-variable _sigstop int "SIGSTOP")
(define-foreign-variable _sigtstp int "SIGTSTP")
(define-foreign-variable _sigxcpu int "SIGXCPU")
(define-foreign-variable _sigxfsz int "SIGXFSZ")
(define-foreign-variable _sigwinch int "SIGWINCH")

(define signal/term _sigterm)
(define signal/kill _sigkill)
(define signal/int _sigint)
(define signal/hup _sighup)
(define signal/fpe _sigfpe)
(define signal/ill _sigill)
(define signal/segv _sigsegv)
(define signal/abrt _sigabrt)
(define signal/trap _sigtrap)
(define signal/quit _sigquit)
(define signal/alrm _sigalrm)
(define signal/vtalrm _sigvtalrm)
(define signal/prof _sigprof)
(define signal/io _sigio)
(define signal/urg _sigurg)
(define signal/chld _sigchld)
(define signal/cont _sigcont)
(define signal/stop _sigstop)
(define signal/tstp _sigtstp)
(define signal/pipe _sigpipe)
(define signal/xcpu _sigxcpu)
(define signal/xfsz _sigxfsz)
(define signal/usr1 _sigusr1)
(define signal/usr2 _sigusr2)
(define signal/winch _sigwinch)

(define signals-list
  (list
    signal/term signal/kill signal/int signal/hup signal/fpe signal/ill
    signal/segv signal/abrt signal/trap signal/quit signal/alrm signal/vtalrm
    signal/prof signal/io signal/urg signal/chld signal/cont signal/stop
    signal/tstp signal/pipe signal/xcpu signal/xfsz signal/usr1 signal/usr2
    signal/winch))

(define set-signal-mask!
  (lambda (sigs)
    (##sys#check-list sigs 'set-signal-mask!)
    (##core#inline "C_sigemptyset" 0)
    (for-each
      (lambda (s)
        (##sys#check-exact s 'set-signal-mask!)
        (##core#inline "C_sigaddset" s) )
      sigs)
    (when (fx< (##core#inline "C_sigprocmask_set" 0) 0)
      (posix-error #:process-error 'set-signal-mask! "cannot set signal mask") ) ) )

(define (signal-mask)
  (let loop ([sigs signals-list] [mask '()])
    (if (null? sigs)
        mask
        (let ([sig (car sigs)])
          (loop (cdr sigs) (if (##core#inline "C_sigismember" sig) (cons sig mask) mask)) ) ) ) )

(define (signal-masked? sig)
  (##sys#check-exact sig 'signal-masked?)
  (##core#inline "C_sigismember" sig) )

(define (signal-mask! sig)
  (##sys#check-exact sig 'signal-mask!)
  (##core#inline "C_sigaddset" sig)
  (when (fx< (##core#inline "C_sigprocmask_block" 0) 0)
      (posix-error #:process-error 'signal-mask! "cannot block signal") )  )

(define (signal-unmask! sig)
  (##sys#check-exact sig 'signal-unmask!)
  (##core#inline "C_sigdelset" sig)
  (when (fx< (##core#inline "C_sigprocmask_unblock" 0) 0)
      (posix-error #:process-error 'signal-unmask! "cannot unblock signal") )  )


;;; Getting system-, group- and user-information:

(define-foreign-variable _uname int "C_uname")
(define-foreign-variable _uname-sysname nonnull-c-string "C_utsname.sysname")
(define-foreign-variable _uname-nodename nonnull-c-string "C_utsname.nodename")
(define-foreign-variable _uname-release nonnull-c-string "C_utsname.release")
(define-foreign-variable _uname-version nonnull-c-string "C_utsname.version")
(define-foreign-variable _uname-machine nonnull-c-string "C_utsname.machine")

(define system-information
  (lambda ()
    (when (fx< _uname 0)
      (##sys#update-errno)
      (##sys#error 'system-information "cannot retrieve system information") )
    (list _uname-sysname
          _uname-nodename
          _uname-release
          _uname-version
          _uname-machine) ) )

(define current-user-id
  (getter-with-setter
   (foreign-lambda int "C_getuid")
   (lambda (id)
     (when (fx< (##core#inline "C_setuid" id) 0)
       (##sys#update-errno)
       (##sys#error 'set-user-id! "cannot set user ID" id) ) )
   "(current-user-id)"))

(define current-effective-user-id
  (getter-with-setter
   (foreign-lambda int "C_geteuid")
   (lambda (id)
    (when (fx< (##core#inline "C_seteuid" id) 0)
      (##sys#update-errno)
      (##sys#error 
	 'effective-user-id!-setter "cannot set effective user ID" id) ) )
   "(current-effective-used-id)"))

(define current-group-id
  (getter-with-setter
   (foreign-lambda int "C_getgid")
   (lambda (id)
    (when (fx< (##core#inline "C_setgid" id) 0)
      (##sys#update-errno)
      (##sys#error 'set-user-id! "cannot set group ID" id) ) )
   "(current-group-id)") )

(define current-effective-group-id
  (getter-with-setter 
   (foreign-lambda int "C_getegid")
   (lambda (id)
    (when (fx< (##core#inline "C_setegid" id) 0)
      (##sys#update-errno)
      (##sys#error 
	 'effective-group-id!-setter "cannot set effective group ID" id) ) )
   "(current-effective-group-id)") )

(define-foreign-variable _user-name nonnull-c-string "C_user->pw_name")
(define-foreign-variable _user-passwd nonnull-c-string "C_user->pw_passwd")
(define-foreign-variable _user-uid int "C_user->pw_uid")
(define-foreign-variable _user-gid int "C_user->pw_gid")
(define-foreign-variable _user-gecos nonnull-c-string "C_user->pw_gecos")
(define-foreign-variable _user-dir c-string "C_user->pw_dir")
(define-foreign-variable _user-shell c-string "C_user->pw_shell")

(define (user-information user #!optional as-vector)
  (let ([r (if (fixnum? user)
               (##core#inline "C_getpwuid" user)
               (begin
                 (##sys#check-string user 'user-information)
                 (##core#inline "C_getpwnam" (##sys#make-c-string user 'user-information)) ) ) ] )
    (and r
         ((if as-vector vector list)
          _user-name
          _user-passwd
          _user-uid
          _user-gid
          _user-gecos
          _user-dir
          _user-shell) ) ) )

(define (current-user-name)
  (car (user-information (current-user-id))) )

(define (current-effective-user-name)
  (car (user-information (current-effective-user-id))) )

(define-foreign-variable _group-name nonnull-c-string "C_group->gr_name")
(define-foreign-variable _group-passwd nonnull-c-string "C_group->gr_passwd")
(define-foreign-variable _group-gid int "C_group->gr_gid")

(define group-member
  (foreign-lambda* c-string ([int i])
    "C_return(C_group->gr_mem[ i ]);") )

(define (group-information group #!optional as-vector)
  (let ([r (if (fixnum? group)
               (##core#inline "C_getgrgid" group)
               (begin
                 (##sys#check-string group 'group-information)
                 (##core#inline "C_getgrnam" (##sys#make-c-string group 'group-information)) ) ) ] )
    (and r
         ((if as-vector vector list)
          _group-name
          _group-passwd
          _group-gid
          (let loop ([i 0])
            (let ([n (group-member i)])
              (if n
                  (cons n (loop (fx+ i 1)))
                  '() ) ) ) ) ) ) )

(define _get-groups
  (foreign-lambda* int ([int n])
    "C_return(getgroups(n, C_groups));") )

(define _ensure-groups
  (foreign-lambda* bool ([int n])
    "if(C_groups != NULL) C_free(C_groups);"
    "C_groups = (gid_t *)C_malloc(sizeof(gid_t) * n);"
    "if(C_groups == NULL) C_return(0);"
    "else C_return(1);") )

(define (get-groups)
  (let ([n (foreign-value "getgroups(0, C_groups)" int)])
    (when (fx< n 0)
      (##sys#update-errno)
      (##sys#error 'get-groups "cannot retrieve supplementary group ids") )
    (unless (_ensure-groups n)
      (##sys#error 'get-groups "out of memory") )
    (when (fx< (_get-groups n) 0)
      (##sys#update-errno)
      (##sys#error 'get-groups "cannot retrieve supplementary group ids") )
    (let loop ([i 0])
      (if (fx>= i n)
          '()
          (cons (##core#inline "C_get_gid" i) (loop (fx+ i 1))) ) ) ) )

(define (set-groups! lst0)
  (unless (_ensure-groups (length lst0))
    (##sys#error 'set-groups! "out of memory") )
  (do ([lst lst0 (##sys#slot lst 1)]
       [i 0 (fx+ i 1)] )
      ((null? lst)
       (when (fx< (##core#inline "C_set_groups" i) 0)
       (##sys#update-errno)
       (##sys#error 'set-groups! "cannot set supplementary group ids" lst0) ) )
    (let ([n (##sys#slot lst 0)])
      (##sys#check-exact n 'set-groups!)
      (##core#inline "C_set_gid" i n) ) ) )

(define initialize-groups
  (let ([init (foreign-lambda int "initgroups" c-string int)])
    (lambda (user id)
      (##sys#check-string user 'initialize-groups)
      (##sys#check-exact id 'initialize-groups)
      (when (fx< (init user id) 0)
      (##sys#update-errno)
      (##sys#error 'initialize-groups "cannot initialize supplementary group ids" user id) ) ) ) )


;;; More errno codes:

(define errno/perm _eperm)
(define errno/noent _enoent)
(define errno/srch _esrch)
(define errno/intr _eintr)
(define errno/io _eio)
(define errno/noexec _enoexec)
(define errno/badf _ebadf)
(define errno/child _echild)
(define errno/nomem _enomem)
(define errno/acces _eacces)
(define errno/fault _efault)
(define errno/busy _ebusy)
(define errno/notdir _enotdir)
(define errno/isdir _eisdir)
(define errno/inval _einval)
(define errno/mfile _emfile)
(define errno/nospc _enospc)
(define errno/spipe _espipe)
(define errno/pipe _epipe)
(define errno/again _eagain)
(define errno/rofs _erofs)
(define errno/exist _eexist)
(define errno/wouldblock _ewouldblock)

(define errno/2big 0)
(define errno/deadlk 0)
(define errno/dom 0)
(define errno/fbig 0)
(define errno/ilseq 0)
(define errno/mlink 0)
(define errno/nametoolong 0)
(define errno/nfile 0)
(define errno/nodev 0)
(define errno/nolck 0)
(define errno/nosys 0)
(define errno/notempty 0)
(define errno/notty 0)
(define errno/nxio 0)
(define errno/range 0)
(define errno/xdev 0)

;;; Permissions and owners:

(define change-file-mode
  (lambda (fname m)
    (##sys#check-string fname 'change-file-mode)
    (##sys#check-exact m 'change-file-mode)
    (when (fx< (##core#inline "C_chmod" (##sys#make-c-string (##sys#expand-home-path fname) 'change-file-mode) m) 0)
      (posix-error #:file-error 'change-file-mode "cannot change file mode" fname m) ) ) )

(define change-file-owner
  (lambda (fn uid gid)
    (##sys#check-string fn 'change-file-owner)
    (##sys#check-exact uid 'change-file-owner)
    (##sys#check-exact gid 'change-file-owner)
    (when (fx< (##core#inline "C_chown" (##sys#make-c-string (##sys#expand-home-path fn) 'change-file-owner) uid gid) 0)
      (posix-error #:file-error 'change-file-owner "cannot change file owner" fn uid gid) ) ) )

(define-foreign-variable _r_ok int "R_OK")
(define-foreign-variable _w_ok int "W_OK")
(define-foreign-variable _x_ok int "X_OK")

(let ()
  (define (check filename acc loc)
    (##sys#check-string filename loc)
    (let ([r (fx= 0 (##core#inline "C_test_access" (##sys#make-c-string (##sys#expand-home-path filename) loc) acc))])
      (unless r (##sys#update-errno))
      r) )
  (set! file-read-access? (lambda (filename) (check filename _r_ok 'file-read-access?)))
  (set! file-write-access? (lambda (filename) (check filename _w_ok 'file-write-access?)))
  (set! file-execute-access? (lambda (filename) (check filename _x_ok 'file-execute-access?))) )

(define (create-session)
  (let ([a (##core#inline "C_setsid" #f)])
    (when (fx< a 0)
      (##sys#update-errno)
      (##sys#error 'create-session "cannot create session") )
    a) )

(define process-group-id
  (getter-with-setter
   (lambda (pid)
     (##sys#check-exact pid 'process-group-id)
     (let ([a (##core#inline "C_getpgid" pid)])
       (when (fx< a 0)
         (##sys#update-errno)
         (##sys#error 'process-group-id "cannot retrieve process group ID" pid) )
       a))
   (lambda (pid pgid)
     (##sys#check-exact pid 'set-process-group-id!)
     (##sys#check-exact pgid 'set-process-group-id!)
     (when (fx< (##core#inline "C_setpgid" pid pgid) 0)
       (##sys#update-errno)
       (##sys#error 'set-process-group-id! "cannot set process group ID" pid pgid) ) )
   "(process-group-id pid)"))


;;; Hard and symbolic links:

(define create-symbolic-link
  (lambda (old new)
    (##sys#check-string old 'create-symbolic-link)
    (##sys#check-string new 'create-symbolic-link)
    (when (fx< (##core#inline
              "C_symlink"
              (##sys#make-c-string (##sys#expand-home-path old) 'create-symbolic-link)
              (##sys#make-c-string (##sys#expand-home-path new) 'create-symbolic-link) )
             0)
      (posix-error #:file-error 'create-symbol-link "cannot create symbolic link" old new) ) ) )

(define-foreign-variable _filename_max int "FILENAME_MAX")

(define read-symbolic-link
  (let ((buf (make-string (fx+ _filename_max 1))))
    (lambda (fname #!optional canonicalize)
      (##sys#check-string fname 'read-symbolic-link)
      (let ((len (##core#inline 
		  "C_do_readlink"
		  (##sys#make-c-string (##sys#expand-home-path fname) 'read-symbolic-link) buf)))
	(if (fx< len 0)
	    (if canonicalize
		fname
		(posix-error #:file-error 'read-symbolic-link "cannot read symbolic link" fname))
	    (let ((pathname (substring buf 0 len)))
	      (if (and canonicalize (symbolic-link? pathname))
		  (read-symbolic-link pathname 'canonicalize)
		  pathname ) ) ) ) ) ) )

(define file-link
  (let ([link (foreign-lambda int "link" c-string c-string)])
    (lambda (old new)
      (##sys#check-string old 'file-link)
      (##sys#check-string new 'file-link)
      (when (fx< (link old new) 0)
      (posix-error #:file-error 'hard-link "could not create hard link" old new) ) ) ) )


(define ##sys#custom-input-port
  (lambda (loc nam fd #!optional (nonblocking? #f) (bufi 1) (on-close void) (more? #f))
    (when nonblocking? (##sys#file-nonblocking! fd) )
    (let ([bufsiz (if (fixnum? bufi) bufi (##sys#size bufi))]
	  [buf (if (fixnum? bufi) (##sys#make-string bufi) bufi)]
	  [buflen 0]
	  [bufpos 0] )
      (let ([ready?
	     (lambda ()
	       (let ((res (##sys#file-select-one fd)))
		 (if (fx= -1 res)
		     (if (or (fx= _errno _ewouldblock)
			     (rx= _errno _eagain))
			 #f
			 (posix-error #:file-error loc "cannot select" fd nam))
		     (fx= 1 res))))]
            [peek
	     (lambda ()
	       (if (fx>= bufpos buflen)
		   #!eof
		   (##core#inline "C_subchar" buf bufpos)) )]
            [fetch
	     (lambda ()
	       (when (fx>= bufpos buflen)
		 (let loop ()
		   (let ([cnt (##core#inline "C_read" fd buf bufsiz)])
		     (cond ((fx= cnt -1)
			    (select _errno
			      ((_ewouldblock _eagain)
			       (##sys#thread-block-for-i/o! ##sys#current-thread fd #:input)
			       (##sys#thread-yield!)
			       (loop) )
			      ((_eintr)
			       (##sys#dispatch-interrupt loop))
			      (else (posix-error #:file-error loc "cannot read" fd nam) )))
			   [(and more? (fx= cnt 0))
					; When "more" keep trying, otherwise read once more
					; to guard against race conditions
			    (if (more?)
				(begin
				  (##sys#thread-yield!)
				  (loop) )
				(let ([cnt (##core#inline "C_read" fd buf bufsiz)])
				  (when (fx= cnt -1)
				    (if (or (fx= _errno _ewouldblock)
					    (fx= _errno _eagain))
					(set! cnt 0)
					(posix-error #:file-error loc "cannot read" fd nam) ) )
				  (set! buflen cnt)
				  (set! bufpos 0) ) )]
			   [else
			    (set! buflen cnt)
			    (set! bufpos 0)]) ) ) ) )] )
	(letrec ([this-port
		  (make-input-port
		   (lambda ()		; read-char
		     (fetch)
		     (let ([ch (peek)])
		       #; ; Allow increment since overflow is far, far away
		       (unless (eof-object? ch) (set! bufpos (fx+ bufpos 1)))
		       (set! bufpos (fx+ bufpos 1))
		       ch ) )
		   (lambda ()		; char-ready?
		     (or (fx< bufpos buflen)
			 (ready?)) )
		   (lambda ()	      ; close
					; Do nothing when closed already
		     (unless (##sys#slot this-port 8)
		       (when (fx< (##core#inline "C_close" fd) 0)
			 (posix-error #:file-error loc "cannot close" fd nam) )
		       (on-close) ) )
		   (lambda ()		; peek-char
		     (fetch)
		     (peek) )
		   (lambda (port n dest start) ; read-string!
		     (let loop ([n (or n (fx- (##sys#size dest) start))] [m 0] [start start])
		       (cond [(eq? 0 n) m]
			     [(fx< bufpos buflen)
                              (let* ([rest (fx- buflen bufpos)]
                                     [n2 (if (fx< n rest) n rest)])
                                (##core#inline "C_substring_copy" buf dest bufpos (fx+ bufpos n2) start)
                                (set! bufpos (fx+ bufpos n2))
                                (loop (fx- n n2) (fx+ m n2) (fx+ start n2)) ) ]
			     [else
                              (fetch)
                              (if (eq? 0 buflen) 
                                  m
                                  (loop n m start) ) ] ) ) )
		   (lambda (port limit)	; read-line
		     (let loop ([str #f])
		       (let ([bumper
			      (lambda (cur ptr)
				(let* ([cnt (fx- cur bufpos)]
				       [dest
					(if (eq? 0 cnt)
					    (or str "")
					    (let ([dest (##sys#make-string cnt)])
					      (##core#inline "C_substring_copy"
							     buf dest bufpos cur 0)
					      (##sys#setislot port 5
							      (fx+ (##sys#slot port 5) cnt))
					      (if str
						  (##sys#string-append str dest)
						  dest ) ) ) ] )
				  (set! bufpos ptr)
				  (cond [(eq? cur ptr) ; no EOL encountered
                                         (fetch)
                                         (values dest (fx< bufpos buflen)) ]
                                        [else ; at EOL
					 (##sys#setislot port 4 (fx+ (##sys#slot port 4) 1))
					 (##sys#setislot port 5 0)
					 (values dest #f) ] ) ) ) ] )
			 (cond [(fx< bufpos buflen)
                                (let-values ([(dest cont?)
                                              (##sys#scan-buffer-line buf buflen bufpos bumper)])
                                  (if cont?
                                      (loop dest)
                                      dest ) ) ]
			       [else
                                (fetch)
                                (if (fx< bufpos buflen)
                                    (loop str)
                                    #!eof) ] ) ) ) )
		   (lambda (port)		; read-buffered
		     (if (fx>= bufpos buflen)
			 ""
			 (let ((str (##sys#substring buf bufpos buflen)))
			   (set! bufpos buflen)
			   str)))
		   ) ] )
	  (set-port-name! this-port nam)
	  this-port ) ) ) ) )

(define ##sys#custom-output-port
  (lambda (loc nam fd #!optional (nonblocking? #f) (bufi 0) (on-close void))
    (when nonblocking? (##sys#file-nonblocking! fd) )
    (letrec ([poke
	      (lambda (str len)
		(let loop ()
		  (let ([cnt (##core#inline "C_write" fd str len)])
		    (cond ((fx= -1 cnt)
			   (select _errno
			     ((_ewouldblock _eagain)
			      (##sys#thread-yield!)
			      (poke str len) )
			     ((_eintr)
			      (##sys#dispatch-interrupt loop))
			     (else
			      (posix-error loc #:file-error "cannot write" fd nam) ) ) )
			  ((fx< cnt len)
			   (poke (##sys#substring str cnt len) (fx- len cnt)) ) ) ) ))]
	     [store
	      (let ([bufsiz (if (fixnum? bufi) bufi (##sys#size bufi))])
		(if (fx= 0 bufsiz)
		    (lambda (str)
		      (when str
			(poke str (##sys#size str)) ) )
		    (let ([buf (if (fixnum? bufi) (##sys#make-string bufi) bufi)]
			  [bufpos 0])
		      (lambda (str)
			(if str
			    (let loop ([rem (fx- bufsiz bufpos)] [start 0] [len (##sys#size str)])
			      (cond [(fx= 0 rem)
				     (poke buf bufsiz)
				     (set! bufpos 0)
				     (loop bufsiz 0 len)]
				    [(fx< rem len)
				     (##core#inline "C_substring_copy" str buf start rem bufpos)
				     (loop 0 rem (fx- len rem))]
				    [else
				     (##core#inline "C_substring_copy" str buf start len bufpos)
				     (set! bufpos (fx+ bufpos len))] ) )
			    (when (fx< 0 bufpos)
			      (poke buf bufpos) ) ) ) ) ) )])
      (letrec ([this-port
		(make-output-port
		 (lambda (str)		; write-string
		   (store str) )
		 (lambda ()	      ; close - do nothing when closed already
		   (unless (##sys#slot this-port 8)
		     (when (fx< (##core#inline "C_close" fd) 0)
		       (posix-error #:file-error loc "cannot close" fd nam) )
		     (on-close) ) )
		 (lambda ()		; flush
		   (store #f) ) )] )
	(set-port-name! this-port nam)
	this-port ) ) ) )


;;; Other file operations:

(define file-truncate
  (lambda (fname off)
    (##sys#check-number off 'file-truncate)
    (when (fx< (cond [(string? fname) (##core#inline "C_truncate" (##sys#make-c-string (##sys#expand-home-path fname) 'file-truncate) off)]
		     [(fixnum? fname) (##core#inline "C_ftruncate" fname off)]
		     [else (##sys#error 'file-truncate "invalid file" fname)] )
	       0)
      (posix-error #:file-error 'file-truncate "cannot truncate file" fname off) ) ) )


;;; Record locking:

(define-foreign-variable _f_wrlck int "F_WRLCK")
(define-foreign-variable _f_rdlck int "F_RDLCK")
(define-foreign-variable _f_unlck int "F_UNLCK")

(let ()
  (define (setup port args loc)
    (let-optionals* args ([start 0]
                          [len #t] )
      (##sys#check-port port loc)
      (##sys#check-number start loc)
      (if (eq? #t len)
          (set! len 0)
          (##sys#check-number len loc) )
      (##core#inline "C_flock_setup" (if (##sys#slot port 1) _f_rdlck _f_wrlck) start len)
      (##sys#make-structure 'lock port start len) ) )
  (define (err msg lock loc)
    (posix-error #:file-error loc msg (##sys#slot lock 1) (##sys#slot lock 2) (##sys#slot lock 3)) )
  (set! file-lock
    (lambda (port . args)
      (let ([lock (setup port args 'file-lock)])
        (if (fx< (##core#inline "C_flock_lock" port) 0)
            (err "cannot lock file" lock 'file-lock)
            lock) ) ) )
  (set! file-lock/blocking
    (lambda (port . args)
      (let ([lock (setup port args 'file-lock/blocking)])
        (if (fx< (##core#inline "C_flock_lockw" port) 0)
            (err "cannot lock file" lock 'file-lock/blocking)
            lock) ) ) )
  (set! file-test-lock
    (lambda (port . args)
      (let ([lock (setup port args 'file-test-lock)])
        (cond [(##core#inline "C_flock_test" port) => (lambda (c) (and (not (fx= c 0)) c))]
              [else (err "cannot unlock file" lock 'file-test-lock)] ) ) ) ) )

(define file-unlock
  (lambda (lock)
    (##sys#check-structure lock 'lock 'file-unlock)
    (##core#inline "C_flock_setup" _f_unlck (##sys#slot lock 2) (##sys#slot lock 3))
    (when (fx< (##core#inline "C_flock_lock" (##sys#slot lock 1)) 0)
      (posix-error #:file-error 'file-unlock "cannot unlock file" lock) ) ) )


;;; FIFOs:

(define create-fifo
  (lambda (fname . mode)
    (##sys#check-string fname 'create-fifo)
    (let ([mode (if (pair? mode) (car mode) (fxior _s_irwxu (fxior _s_irwxg _s_irwxo)))])
      (##sys#check-exact mode 'create-fifo)
      (when (fx< (##core#inline "C_mkfifo" (##sys#make-c-string (##sys#expand-home-path fname) 'create-fifo) mode) 0)
      (posix-error #:file-error 'create-fifo "cannot create FIFO" fname mode) ) ) ) )

(define fifo?
  (lambda (filename)
    (##sys#check-string filename 'fifo?)
    (case (##core#inline 
	   "C_i_fifo_p"
	   (##sys#make-c-string (##sys#expand-home-path filename) 'fifo?))
      ((#t) #t)
      ((#f) #f)
      ((0) (##sys#signal-hook #:file-error 'fifo? "file does not exist" filename) )
      (else
       (posix-error 
	#:file-error 'fifo?
	"system error while trying to access file" filename) ) ) ) )


;;; Environment access:

(define setenv
  (lambda (var val)
    (##sys#check-string var 'setenv)
    (##sys#check-string val 'setenv)
    (##core#inline "C_setenv" (##sys#make-c-string var 'setenv) (##sys#make-c-string val 'setenv))
    (##core#undefined) ) )

(define (unsetenv var)
  (##sys#check-string var 'unsetenv)
  (##core#inline "C_unsetenv" (##sys#make-c-string var 'unsetenv))
  (##core#undefined) )

(define get-environment-variables
  (let ([get (foreign-lambda c-string "C_getenventry" int)])
    (lambda ()
      (let loop ([i 0])
        (let ([entry (get i)])
          (if entry
              (let scan ([j 0])
                (if (char=? #\= (##core#inline "C_subchar" entry j))
                    (cons (cons (##sys#substring entry 0 j)
                                (##sys#substring entry (fx+ j 1) (##sys#size entry)))
                          (loop (fx+ i 1)))
                    (scan (fx+ j 1)) ) )
              '() ) ) ) ) ) )


;;; Memory mapped I/O:

(define-foreign-variable _prot_read int "PROT_READ")
(define-foreign-variable _prot_write int "PROT_WRITE")
(define-foreign-variable _prot_exec int "PROT_EXEC")
(define-foreign-variable _prot_none int "PROT_NONE")

(define prot/read _prot_read)
(define prot/write _prot_write)
(define prot/exec _prot_exec)
(define prot/none _prot_none)

(define-foreign-variable _map_fixed int "MAP_FIXED")
(define-foreign-variable _map_shared int "MAP_SHARED")
(define-foreign-variable _map_private int "MAP_PRIVATE")
(define-foreign-variable _map_anonymous int "MAP_ANON")
(define-foreign-variable _map_file int "MAP_FILE")

(define map/fixed _map_fixed)
(define map/shared _map_shared)
(define map/private _map_private)
(define map/anonymous _map_anonymous)
(define map/file _map_file)

(define map-file-to-memory
  (let ([mmap (foreign-lambda c-pointer "mmap" c-pointer integer int int int integer)] )
    (lambda (addr len prot flag fd . off)
      (let ([addr (if (not addr) (##sys#null-pointer) addr)]
            [off (if (pair? off) (car off) 0)] )
        (unless (and (##core#inline "C_blockp" addr) (##core#inline "C_specialp" addr))
	  (##sys#signal-hook #:type-error 'map-file-to-memory "bad argument type - not a foreign pointer" addr) )
        (let ([addr2 (mmap addr len prot flag fd off)])
          (when (eq? -1 (##sys#pointer->address addr2))
	    (posix-error #:file-error 'map-file-to-memory "cannot map file to memory" addr len prot flag fd off) )
          (##sys#make-structure 'mmap addr2 len) ) ) ) ) )

(define unmap-file-from-memory
  (let ([munmap (foreign-lambda int "munmap" c-pointer integer)] )
    (lambda (mmap . len)
      (##sys#check-structure mmap 'mmap 'unmap-file-from-memory)
      (let ([len (if (pair? len) (car len) (##sys#slot mmap 2))])
        (unless (eq? 0 (munmap (##sys#slot mmap 1) len))
	  (posix-error #:file-error 'unmap-file-from-memory "cannot unmap file from memory" mmap len) ) ) ) ) )

(define (memory-mapped-file-pointer mmap)
  (##sys#check-structure mmap 'mmap 'memory-mapped-file-pointer)
  (##sys#slot mmap 1) )

(define (memory-mapped-file? x)
  (##sys#structure? x 'mmap) )

;;; Time related things:

(define time->string
  (let ([asctime (foreign-lambda c-string "C_asctime" scheme-object)]
        [strftime (foreign-lambda c-string "C_strftime" scheme-object scheme-object)])
    (lambda (tm #!optional fmt)
      (check-time-vector 'time->string tm)
      (if fmt
          (begin
            (##sys#check-string fmt 'time->string)
            (or (strftime tm (##sys#make-c-string fmt 'time->string))
                (##sys#error 'time->string "time formatting overflows buffer" tm)) )
          (let ([str (asctime tm)])
            (if str
                (##sys#substring str 0 (fx- (##sys#size str) 1))
                (##sys#error 'time->string "cannot convert time vector to string" tm) ) ) ) ) ) )

(define string->time
  (let ([strptime (foreign-lambda scheme-object "C_strptime" scheme-object scheme-object scheme-object)])
    (lambda (tim #!optional (fmt "%a %b %e %H:%M:%S %Z %Y"))
      (##sys#check-string tim 'string->time)
      (##sys#check-string fmt 'string->time)
      (strptime (##sys#make-c-string tim 'string->time) (##sys#make-c-string fmt) (make-vector 10 #f)) ) ) )

(define (utc-time->seconds tm)
  (check-time-vector 'utc-time->seconds tm)
  (let ((t (##core#inline_allocate ("C_a_timegm" 4) tm)))
    (if (fp= -1.0 t)
	(##sys#error 'utc-time->seconds "cannot convert time vector to seconds" tm) 
	t)))

(define local-timezone-abbreviation
  (foreign-lambda* c-string ()
   "\n#if !defined(__CYGWIN__) && !defined(__SVR4) && !defined(__uClinux__) && !defined(__hpux__)\n"
   "time_t clock = time(NULL);"
   "struct tm *ltm = C_localtime(&clock);"
   "char *z = ltm ? (char *)ltm->tm_zone : 0;"
   "\n#else\n"
   "char *z = (daylight ? tzname[1] : tzname[0]);"
   "\n#endif\n"
   "C_return(z);") )


;;; Other things:

(define _exit
  (let ([ex0 (foreign-lambda void "_exit" int)])
    (lambda code
      (ex0 (if (pair? code) (car code) 0)) ) ) )

(define set-alarm! (foreign-lambda int "C_alarm" int))

(define-foreign-variable _iofbf int "_IOFBF")
(define-foreign-variable _iolbf int "_IOLBF")
(define-foreign-variable _ionbf int "_IONBF")
(define-foreign-variable _bufsiz int "BUFSIZ")

(define set-buffering-mode!
  (lambda (port mode . size)
    (##sys#check-port port 'set-buffering-mode!)
    (let ([size (if (pair? size) (car size) _bufsiz)]
	  [mode (case mode
		  [(#:full) _iofbf]
		  [(#:line) _iolbf]
		  [(#:none) _ionbf]
		  [else (##sys#error 'set-buffering-mode! "invalid buffering-mode" mode port)] ) ] )
      (##sys#check-exact size 'set-buffering-mode!)
      (when (fx< (if (eq? 'stream (##sys#slot port 7))
		     (##core#inline "C_setvbuf" port mode size)
		     -1)
		 0)
	(##sys#error 'set-buffering-mode! "cannot set buffering mode" port mode size) ) ) ) )

(define (terminal-port? port)
  (##sys#check-open-port port 'terminal-port?)
  (let ([fp (##sys#peek-unsigned-integer port 0)])
    (and (not (eq? 0 fp)) (##core#inline "C_tty_portp" port) ) ) )

(define (##sys#terminal-check caller port)
  (##sys#check-open-port port caller)
  (unless (and (eq? 'stream (##sys#slot port 7))
	       (##core#inline "C_tty_portp" port))
	  (##sys#error caller "port is not connected to a terminal" port)))

(define terminal-name
  (let ([ttyname (foreign-lambda nonnull-c-string "ttyname" int)] )
    (lambda (port)
      (##sys#terminal-check 'terminal-name port)
      (ttyname (##core#inline "C_C_fileno" port) ) ) ) )

(define terminal-size
  (let ((ttysize (foreign-lambda int "get_tty_size" int
				 (nonnull-c-pointer int)
				 (nonnull-c-pointer int))))
    (lambda (port)
      (##sys#terminal-check 'terminal-size port)
      (let-location ((columns int)
		     (rows int))
	(if (fx= 0
		 (ttysize (##core#inline "C_C_fileno" port)
			  (location columns)
			  (location rows)))
	    (values columns rows)
	    (posix-error #:error 'terminal-size
			 "Unable to get size of terminal" port))))))
  
(define get-host-name
  (let ([getit
	 (foreign-lambda* c-string ()
	   "if(gethostname(C_hostbuf, 256) == -1) C_return(NULL);"
	   "else C_return(C_hostbuf);") ] )
    (lambda ()
      (let ([host (getit)])
        (unless host
          (posix-error #:error 'get-host-name "cannot retrieve host-name") )
        host) ) ) )


;;; Process handling:

(define process-fork
  (let ([fork (foreign-lambda int "C_fork")])
    (lambda thunk
      (let ([pid (fork)])
      (cond [(fx= -1 pid) (posix-error #:process-error 'process-fork "cannot create child process")]
            [(and (pair? thunk) (fx= pid 0))
             ((car thunk))
             ((foreign-lambda void "_exit" int) 0) ]
            [else pid] ) ) ) ) )

(define process-execute
  ;; NOTE: We use c-string here instead of scheme-object.
  ;; Because set_exec_* make a copy, this implies a double copy.
  ;; At least it's secure, we can worry about performance later, if at all
  (let ([setarg (foreign-lambda void "C_set_exec_arg" int c-string int)]
        [freeargs (foreign-lambda void "C_free_exec_args")]
        [setenv (foreign-lambda void "C_set_exec_env" int c-string int)]
        [freeenv (foreign-lambda void "C_free_exec_env")]
        [pathname-strip-directory pathname-strip-directory] )
    (lambda (filename #!optional (arglist '()) envlist)
      (##sys#check-string filename 'process-execute)
      (##sys#check-list arglist 'process-execute)
      (let ([s (pathname-strip-directory filename)])
        (setarg 0 s (##sys#size s)) )
      (do ([al arglist (cdr al)]
           [i 1 (fx+ i 1)] )
          ((null? al)
           (setarg i #f 0)
           (when envlist
             (##sys#check-list envlist 'process-execute)
             (do ([el envlist (cdr el)]
                  [i 0 (fx+ i 1)] )
                 ((null? el) (setenv i #f 0))
               (let ([s (car el)])
                 (##sys#check-string s 'process-execute)
                 (setenv i s (##sys#size s)) ) ) )
           (let* ([prg (##sys#make-c-string (##sys#expand-home-path filename) 'process-execute)]
                  [r (if envlist
                         (##core#inline "C_execve" prg)
                         (##core#inline "C_execvp" prg) )] )
             (when (fx= r -1)
               (freeargs)
               (freeenv)
               (posix-error #:process-error 'process-execute "cannot execute process" filename) ) ) )
        (let ([s (car al)])
          (##sys#check-string s 'process-execute)
          (setarg i s (##sys#size s)) ) ) ) ) )

(define-foreign-variable _wnohang int "WNOHANG")
(define-foreign-variable _wait-status int "C_wait_status")

(define (##sys#process-wait pid nohang)
  (let* ([res (##core#inline "C_waitpid" pid (if nohang _wnohang 0))]
	 [norm (##core#inline "C_WIFEXITED" _wait-status)] )
    (if (and (fx= res -1) (fx= _errno _eintr))
	(##sys#dispatch-interrupt
         (lambda () (##sys#process-wait pid nohang)))
	(values
	 res
	 norm
	 (cond [norm (##core#inline "C_WEXITSTATUS" _wait-status)]
	       [(##core#inline "C_WIFSIGNALED" _wait-status)
		(##core#inline "C_WTERMSIG" _wait-status)]
	       [else (##core#inline "C_WSTOPSIG" _wait-status)] ) )) ) )

(define parent-process-id (foreign-lambda int "C_getppid"))

(define sleep (foreign-lambda int "C_sleep" int))

(define process-signal
  (lambda (id . sig)
    (let ([sig (if (pair? sig) (car sig) _sigterm)])
      (##sys#check-exact id 'process-signal)
      (##sys#check-exact sig 'process-signal)
      (let ([r (##core#inline "C_kill" id sig)])
      (when (fx= r -1) (posix-error #:process-error 'process-signal "could not send signal to process" id sig) ) ) ) ) )

(define (##sys#shell-command)
  (or (get-environment-variable "SHELL") "/bin/sh") )

(define (##sys#shell-command-arguments cmdlin)
  (list "-c" cmdlin) )

(define process-run
  (lambda (f . args)
    (let ([args (if (pair? args) (car args) #f)]
	  [pid (process-fork)] )
      (cond [(not (eq? 0 pid)) pid]
	    [args (process-execute f args)]
	    [else
	     (process-execute (##sys#shell-command) (##sys#shell-command-arguments f)) ] ) ) ) )

;;; Run subprocess connected with pipes:

;; ##sys#process
; loc            caller procedure symbol
; cmd            pathname or commandline
; args           string-list or '()
; env            string-list or #f
; stdoutf        #f then share, or #t then create
; stdinf         #f then share, or #t then create
; stderrf        #f then share, or #t then create
;
; (values stdin-input-port? stdout-output-port? pid stderr-input-port?)
; where stdin-input-port?, etc. is a port or #f, indicating no port created.

(define-constant DEFAULT-INPUT-BUFFER-SIZE 256)
(define-constant DEFAULT-OUTPUT-BUFFER-SIZE 0)

;FIXME process-execute, process-fork don't show parent caller

(define ##sys#process
  (let (
      [replace-fd
        (lambda (loc fd stdfd)
          (unless (fx= stdfd fd)
            (duplicate-fileno fd stdfd)
            (file-close fd) ) )] )
    (let (
        [make-on-close
          (lambda (loc pid clsvec idx idxa idxb)
            (lambda ()
              (vector-set! clsvec idx #t)
              (when (and (vector-ref clsvec idxa) (vector-ref clsvec idxb))
                (receive [_ flg cod] (process-wait pid)
                  (unless flg
                    (##sys#signal-hook #:process-error loc
                      "abnormal process exit" pid cod)) ) ) ) )]
        [needed-pipe
          (lambda (loc port)
            (and port
                 (receive [i o] (create-pipe) (cons i o))) )]
        [connect-parent
          (lambda (loc pipe port fd)
            (and port
                 (let ([usefd (car pipe)] [clsfd (cdr pipe)])
                   (file-close clsfd)
                   usefd) ) )]
        [connect-child
          (lambda (loc pipe port stdfd)
            (when port
              (let ([usefd (car pipe)] [clsfd (cdr pipe)])
                (file-close clsfd)
                (replace-fd loc usefd stdfd)) ) )] )
      (let (
          [spawn
            (let ([swapped-ends
                    (lambda (pipe)
                      (and pipe
                           (cons (cdr pipe) (car pipe)) ) )])
              (lambda (loc cmd args env stdoutf stdinf stderrf)
                (let ([ipipe (needed-pipe loc stdinf)]
                      [opipe (needed-pipe loc stdoutf)]
                      [epipe (needed-pipe loc stderrf)])
                  (values
                    ipipe (swapped-ends opipe) epipe
                    (process-fork
                      (lambda ()
                        (connect-child loc opipe stdinf fileno/stdin)
                        (connect-child loc (swapped-ends ipipe) stdoutf fileno/stdout)
                        (connect-child loc (swapped-ends epipe) stderrf fileno/stderr)
                        (process-execute cmd args env)))) ) ) )]
          [input-port
            (lambda (loc pid cmd pipe stdf stdfd on-close)
              (and-let* ([fd (connect-parent loc pipe stdf stdfd)])
                (##sys#custom-input-port loc cmd fd #t DEFAULT-INPUT-BUFFER-SIZE on-close) ) )]
          [output-port
            (lambda (loc pid cmd pipe stdf stdfd on-close)
              (and-let* ([fd (connect-parent loc pipe stdf stdfd)])
                (##sys#custom-output-port loc cmd fd #t DEFAULT-OUTPUT-BUFFER-SIZE on-close) ) )] )
        (lambda (loc cmd args env stdoutf stdinf stderrf)
          (receive [inpipe outpipe errpipe pid]
                     (spawn loc cmd args env stdoutf stdinf stderrf)
            ;When shared assume already "closed", since only created ports
            ;should be explicitly closed, and when one is closed we want
            ;to wait.
            (let ([clsvec (vector (not stdinf) (not stdoutf) (not stderrf))])
              (values
                (input-port loc pid cmd inpipe stdinf fileno/stdin
                  (make-on-close loc pid clsvec 0 1 2))
                (output-port loc pid cmd outpipe stdoutf fileno/stdout
                  (make-on-close loc pid clsvec 1 0 2))
                pid
                (input-port loc pid cmd errpipe stderrf fileno/stderr
                  (make-on-close loc pid clsvec 2 0 1)) ) ) ) ) ) ) ) )

;;; Run subprocess connected with pipes:

(define process)
(define process*)

(let ((%process
        (lambda (loc err? cmd args env k)
          (let ([chkstrlst
                 (lambda (lst)
                   (##sys#check-list lst loc)
                   (for-each (cut ##sys#check-string <> loc) lst) )])
            (##sys#check-string cmd loc)
            (if args
                (chkstrlst args)
                (begin
                  (set! args (##sys#shell-command-arguments cmd))
                  (set! cmd (##sys#shell-command)) ) )
            (when env (chkstrlst env))
            (##sys#call-with-values 
	     (lambda () (##sys#process loc cmd args env #t #t err?))
	     k)))))
  (set! process
    (lambda (cmd #!optional args env)
      (%process 
       'process #f cmd args env
       (lambda (i o p e) (values i o p)))))
  (set! process*
    (lambda (cmd #!optional args env)
      (%process
       'process* #t cmd args env
       values))))


;;; chroot:

(define set-root-directory!
  (let ([chroot (foreign-lambda int "chroot" c-string)])
    (lambda (dir)
      (##sys#check-string dir 'set-root-directory!)
      (when (fx< (chroot dir) 0)
        (posix-error #:file-error 'set-root-directory! "unable to change root directory" dir) ) ) ) )
