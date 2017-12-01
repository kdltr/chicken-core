/* runtime.c - Runtime code for compiler generated executables
;
; Copyright (c) 2008-2017, The CHICKEN Team
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
*/


#include "chicken.h"
#include <assert.h>
#include <errno.h>
#include <float.h>
#include <signal.h>
#include <sys/stat.h>
#include <strings.h>

#ifdef HAVE_SYSEXITS_H
# include <sysexits.h>
#endif

#ifdef __ANDROID__
# include <android/log.h>
#endif

#if !defined(PIC)
# define NO_DLOAD2
#endif

#ifndef NO_DLOAD2
# ifdef HAVE_DLFCN_H
#  include <dlfcn.h>
# endif

# ifdef HAVE_DL_H
#  include <dl.h>
# endif
#endif

#ifndef EX_SOFTWARE
# define EX_SOFTWARE  70
#endif

#ifndef EOVERFLOW
# define EOVERFLOW  0
#endif

/* TODO: Include sys/select.h? Windows doesn't seem to have it... */
#ifndef NO_POSIX_POLL
#  include <poll.h>
#endif

#if !defined(C_NONUNIX)

# include <sys/time.h>
# include <sys/resource.h>
# include <sys/wait.h>

/* ITIMER_PROF is more precise, but Cygwin doesn't support it... */
# ifdef __CYGWIN__
#  define C_PROFILE_SIGNAL SIGALRM
#  define C_PROFILE_TIMER  ITIMER_REAL
# else
#  define C_PROFILE_SIGNAL SIGPROF
#  define C_PROFILE_TIMER  ITIMER_PROF
# endif

#else

# define C_PROFILE_SIGNAL -1          /* Stupid way to avoid error */

#ifdef ECOS
#include <cyg/kernel/kapi.h>
static C_TLS int timezone;
#define NSIG                          32
#endif

#endif

#ifndef RTLD_GLOBAL
# define RTLD_GLOBAL                   0
#endif

#ifndef RTLD_NOW
# define RTLD_NOW                      0
#endif

#ifndef RTLD_LOCAL
# define RTLD_LOCAL                    0
#endif

#ifndef RTLD_LAZY
# define RTLD_LAZY                     0
#endif

#if defined(_WIN32) && !defined(__CYGWIN__)
/* Include winsock2 to get select() for check_fd_ready() */
# include <winsock2.h>
# include <windows.h>
#endif

#ifdef HAVE_CONFIG_H
# ifdef PACKAGE
#  undef PACKAGE
# endif
# ifdef VERSION
#  undef VERSION
# endif
# include <chicken-config.h>

# ifndef HAVE_ALLOCA
#  error this package requires "alloca()"
# endif
#endif

/* Parameters: */

#define RELAX_MULTIVAL_CHECK

#ifdef C_SIXTY_FOUR
# define DEFAULT_STACK_SIZE            (1024 * 1024)
#else
# define DEFAULT_STACK_SIZE            (256 * 1024)
#endif

#define DEFAULT_SYMBOL_TABLE_SIZE      2999
#define DEFAULT_HEAP_SIZE              DEFAULT_STACK_SIZE
#define MINIMAL_HEAP_SIZE              DEFAULT_STACK_SIZE
#define DEFAULT_MAXIMAL_HEAP_SIZE      0x7ffffff0
#define DEFAULT_HEAP_GROWTH            200
#define DEFAULT_HEAP_SHRINKAGE         50
#define DEFAULT_HEAP_SHRINKAGE_USED    25
#define DEFAULT_FORWARDING_TABLE_SIZE  32
#define DEFAULT_LOCATIVE_TABLE_SIZE    32
#define DEFAULT_COLLECTIBLES_SIZE      1024
#define DEFAULT_TRACE_BUFFER_SIZE      16
#define MIN_TRACE_BUFFER_SIZE          3

#define MAX_HASH_PREFIX                64

#define WEAK_TABLE_SIZE                997
#define WEAK_HASH_ITERATIONS           4
#define WEAK_HASH_DISPLACEMENT         7
#define WEAK_COUNTER_MASK              3
#define WEAK_COUNTER_MAX               2

#define DEFAULT_TEMPORARY_STACK_SIZE   256
#define STRING_BUFFER_SIZE             4096
#define DEFAULT_MUTATION_STACK_SIZE    1024
#define PROFILE_TABLE_SIZE             1024

#define MAX_PENDING_INTERRUPTS         100

#ifdef C_DOUBLE_IS_32_BITS
# define FLONUM_PRINT_PRECISION         7
#else
# define FLONUM_PRINT_PRECISION         15
#endif

#define WORDS_PER_FLONUM               C_SIZEOF_FLONUM
#define INITIAL_TIMER_INTERRUPT_PERIOD 10000
#define HDUMP_TABLE_SIZE               1001

/* only for relevant for Windows: */

#define MAXIMAL_NUMBER_OF_COMMAND_LINE_ARGUMENTS 256


/* Constants: */

#ifdef C_SIXTY_FOUR
# define ALIGNMENT_HOLE_MARKER         ((C_word)0xfffffffffffffffeL)
# define FORWARDING_BIT_SHIFT          63
# define UWORD_FORMAT_STRING           "0x%016lx"
# define UWORD_COUNT_FORMAT_STRING     "%lu"
#else
# define ALIGNMENT_HOLE_MARKER         ((C_word)0xfffffffe)
# define FORWARDING_BIT_SHIFT          31
# define UWORD_FORMAT_STRING           "0x%08x"
# define UWORD_COUNT_FORMAT_STRING     "%u"
#endif

#ifdef C_LLP
# define LONG_FORMAT_STRING            "%lld"
#else
# define LONG_FORMAT_STRING            "%ld"
#endif

#define GC_MINOR           0
#define GC_MAJOR           1
#define GC_REALLOC         2


/* Macros: */

#define nmax(x, y)                   ((x) > (y) ? (x) : (y))
#define nmin(x, y)                   ((x) < (y) ? (x) : (y))
#define percentage(n, p)             ((C_long)(((double)(n) * (double)p) / 100))

#define is_fptr(x)                   (((x) & C_GC_FORWARDING_BIT) != 0)
#define ptr_to_fptr(x)               ((((x) >> FORWARDING_BIT_SHIFT) & 1) | C_GC_FORWARDING_BIT | ((x) & ~1))
#define fptr_to_ptr(x)               (((C_uword)(x) << FORWARDING_BIT_SHIFT) | ((x) & ~(C_GC_FORWARDING_BIT | 1)))

#define C_check_flonum(x, w)        if(C_immediatep(x) || C_block_header(x) != C_FLONUM_TAG) \
                                       barf(C_BAD_ARGUMENT_TYPE_NO_FLONUM_ERROR, w, x);
#define C_check_real(x, w, v)       if(((x) & C_FIXNUM_BIT) != 0) v = C_unfix(x); \
                                     else if(C_immediatep(x) || C_block_header(x) != C_FLONUM_TAG) \
                                       barf(C_BAD_ARGUMENT_TYPE_NO_NUMBER_ERROR, w, x); \
                                     else v = C_flonum_magnitude(x);


/* these could be shorter in unsafe mode: */
#define C_check_int(x, f, n, w)     if(((x) & C_FIXNUM_BIT) != 0) n = C_unfix(x); \
                                     else if(C_immediatep(x) || C_block_header(x) != C_FLONUM_TAG) \
                                       barf(C_BAD_ARGUMENT_TYPE_NO_NUMBER_ERROR, w, x); \
                                     else { double _m; \
                                       f = C_flonum_magnitude(x); \
                                       if(modf(f, &_m) != 0.0 || f < C_WORD_MIN || f > C_WORD_MAX) \
                                         barf(C_BAD_ARGUMENT_TYPE_NO_INTEGER_ERROR, w, x); \
                                       else n = (C_word)f; \
                                     }

#ifdef BITWISE_UINT_ONLY
#define C_check_uint(x, f, n, w)    if(((x) & C_FIXNUM_BIT) != 0) n = C_unfix(x); \
                                     else if(C_immediatep(x) || C_block_header(x) != C_FLONUM_TAG) \
                                       barf(C_BAD_ARGUMENT_TYPE_NO_NUMBER_ERROR, w, x); \
                                     else { double _m; \
                                       f = C_flonum_magnitude(x); \
                                       if(modf(f, &_m) != 0.0 || f < 0 || f > C_UWORD_MAX) \
                                         barf(C_BAD_ARGUMENT_TYPE_NO_UINTEGER_ERROR, w, x); \
                                       else n = (C_uword)f; \
                                     }
#else
#define C_check_uint(x, f, n, w)    if(((x) & C_FIXNUM_BIT) != 0) n = C_unfix(x); \
                                      else if(C_immediatep(x) || C_block_header(x) != C_FLONUM_TAG) \
                                        barf(C_BAD_ARGUMENT_TYPE_NO_NUMBER_ERROR, w, x); \
                                      else { double _m; \
                                        f = C_flonum_magnitude(x); \
                                        if(modf(f, &_m) != 0.0 || f > C_UWORD_MAX) \
                                          barf(C_BAD_ARGUMENT_TYPE_NO_UINTEGER_ERROR, w, x); \
                                        else n = (C_uword)f; \
                                      }
#endif

#ifdef C_SIXTY_FOUR
#define C_limit_fixnum(n)            ((n) & C_MOST_POSITIVE_FIXNUM)
#else
#define C_limit_fixnum(n)            (n)
#endif

#define C_pte(name)                  pt[ i ].id = #name; pt[ i++ ].ptr = (void *)name;

#ifndef SIGBUS
# define SIGBUS                      0
#endif


/* Type definitions: */

typedef struct lf_list_struct
{
  C_word *lf;
  int count;
  struct lf_list_struct *next, *prev;
  C_PTABLE_ENTRY *ptable;
  void *module_handle;
  char *module_name;
} LF_LIST;

typedef struct weak_table_entry_struct
{
  C_word item,			/* item weakly held (symbol) */
         container;		/* object holding reference to symbol, lowest 3 bits are */
} WEAK_TABLE_ENTRY;		/*   also used as a counter, saturated at 2 or more */

typedef struct finalizer_node_struct
{
  struct finalizer_node_struct
    *next,
    *previous;
  C_word
    item,
    finalizer;
} FINALIZER_NODE;

typedef struct trace_info_struct
{
  C_char *raw;
  C_word cooked1, cooked2, thread;
} TRACE_INFO;

typedef struct hdump_bucket_struct
{
  C_word key;
  int count, total;
  struct hdump_bucket_struct *next;
} HDUMP_BUCKET;

typedef struct profile_bucket_struct
{
  C_char *key;
  C_uword sample_count; /* Multiplied by profile freq = time spent */
  C_uword call_count;   /* Distinct calls seen while sampling */
  struct profile_bucket_struct *next;
} PROFILE_BUCKET;


/* Variables: */

C_TLS C_word
  *C_temporary_stack,
  *C_temporary_stack_bottom,
  *C_temporary_stack_limit,
  *C_stack_limit,         /* "Soft" limit, may be reset to force GC */
  *C_stack_hard_limit;    /* Actual stack limit */
C_TLS C_long
  C_timer_interrupt_counter,
  C_initial_timer_interrupt_period;
C_TLS C_byte 
  *C_fromspace_top,
  *C_fromspace_limit;
#ifdef HAVE_SIGSETJMP
C_TLS sigjmp_buf C_restart;
#else
C_TLS jmp_buf C_restart;
#endif
C_TLS void *C_restart_trampoline;
C_TLS C_word C_restart_c;
C_TLS int C_entry_point_status;
C_TLS int (*C_gc_mutation_hook)(C_word *slot, C_word val);
C_TLS void (*C_gc_trace_hook)(C_word *var, int mode);
C_TLS void (*C_panic_hook)(C_char *msg) = NULL;
C_TLS void (*C_pre_gc_hook)(int mode) = NULL;
C_TLS void (*C_post_gc_hook)(int mode, C_long ms) = NULL;
C_TLS C_word (*C_debugger_hook)(C_DEBUG_INFO *cell, C_word c, C_word *av, C_char *cloc, int cln) = NULL;

C_TLS int
  C_gui_mode = 0,
  C_abort_on_thread_exceptions,
  C_enable_repl,
  C_interrupts_enabled,
  C_disable_overflow_check,
#ifdef C_COLLECT_ALL_SYMBOLS
  C_enable_gcweak = 1,
#else
  C_enable_gcweak = 0,
#endif
  C_heap_size_is_fixed,
  C_trace_buffer_size = DEFAULT_TRACE_BUFFER_SIZE,
  C_max_pending_finalizers = C_DEFAULT_MAX_PENDING_FINALIZERS,
  C_debugging = 0,
  C_main_argc;
C_TLS C_uword 
  C_heap_growth,
  C_heap_shrinkage;
C_TLS C_uword C_maximal_heap_size;
C_TLS time_t
  C_startup_time_seconds,
  profile_frequency = 10000;
C_TLS char 
  **C_main_argv,
  *C_dlerror;

static C_TLS TRACE_INFO
  *trace_buffer,
  *trace_buffer_limit,
  *trace_buffer_top;

static C_TLS C_byte 
  *heapspace1, 
  *heapspace2,
  *fromspace_start,
  *tospace_start,
  *tospace_top,
  *tospace_limit,
  *new_tospace_start,
  *new_tospace_top,
  *new_tospace_limit,
  *heap_scan_top;
static C_TLS C_uword
  heapspace1_size,
  heapspace2_size,
  heap_size,
  temporary_stack_size,
  fixed_temporary_stack_size = 0,
  maximum_heap_usage;
static C_TLS C_char
  buffer[ STRING_BUFFER_SIZE ],
  *private_repository = NULL,
  *current_module_name,
  *save_string;
static C_TLS C_SYMBOL_TABLE
  *symbol_table,
  *symbol_table_list;
static C_TLS C_word 
  **collectibles,
  **collectibles_top,
  **collectibles_limit,
  **mutation_stack_bottom,
  **mutation_stack_limit,
  **mutation_stack_top,
  *stack_bottom,
  *locative_table,
  error_location,
  interrupt_hook_symbol,
  current_thread_symbol,
  error_hook_symbol,
  pending_finalizers_symbol,
  callback_continuation_stack_symbol,
  *forwarding_table;
static C_TLS int 
  trace_buffer_full,
  forwarding_table_size,
  return_to_host,
  page_size,
  show_trace,
  fake_tty_flag,
  debug_mode,
  dump_heap_on_exit,
  gc_bell,
  gc_report_flag = 0,
  gc_mode,
  gc_count_1,
  gc_count_1_total,
  gc_count_2,
  weak_table_randomization,
  stack_size_changed,
  dlopen_flags,
  heap_size_changed,
  chicken_is_running,
  chicken_ran_once,
  pass_serious_signals = 1,
  callback_continuation_level;
static volatile C_TLS int
  serious_signal_occurred = 0,
  profiling = 0;
static C_TLS unsigned int
  mutation_count,
  tracked_mutation_count,
  stack_check_demand,
  stack_size;
static C_TLS int chicken_is_initialized;
#ifdef HAVE_SIGSETJMP
static C_TLS sigjmp_buf gc_restart;
#else
static C_TLS jmp_buf gc_restart;
#endif
static C_TLS double
  timer_start_ms,
  gc_ms,
  timer_accumulated_gc_ms,
  interrupt_time,
  last_interrupt_latency;
static C_TLS LF_LIST *lf_list;
static C_TLS int signal_mapping_table[ NSIG ];
static C_TLS int
  locative_table_size,
  locative_table_count,
  live_finalizer_count,
  allocated_finalizer_count,
  pending_finalizer_count,
  callback_returned_flag;
static C_TLS WEAK_TABLE_ENTRY *weak_item_table;
static C_TLS C_GC_ROOT *gc_root_list = NULL;
static C_TLS FINALIZER_NODE 
  *finalizer_list,
  *finalizer_free_list,
  **pending_finalizer_indices;
static C_TLS void *current_module_handle;
static C_TLS int flonum_print_precision = FLONUM_PRINT_PRECISION;
static C_TLS HDUMP_BUCKET **hdump_table;
static C_TLS PROFILE_BUCKET
  *next_profile_bucket = NULL,
  **profile_table = NULL;
static C_TLS int 
  pending_interrupts[ MAX_PENDING_INTERRUPTS ],
  pending_interrupts_count,
  handling_interrupts;


/* Prototypes: */

static void parse_argv(C_char *cmds);
static void initialize_symbol_table(void);
static void global_signal_handler(int signum);
static C_word arg_val(C_char *arg);
static void barf(int code, char *loc, ...) C_noret;
static void panic(C_char *msg) C_noret;
static void usual_panic(C_char *msg) C_noret;
static void horror(C_char *msg) C_noret;
static void C_fcall really_mark(C_word *x) C_regparm;
static WEAK_TABLE_ENTRY *C_fcall lookup_weak_table_entry(C_word item, C_word container) C_regparm;
static C_cpsproc(values_continuation) C_noret;
static C_word add_symbol(C_word **ptr, C_word key, C_word string, C_SYMBOL_TABLE *stable);
static C_regparm int C_fcall C_in_new_heapp(C_word x);
static C_word C_fcall hash_string(int len, C_char *str, C_word m, C_word r, int ci) C_regparm;
static C_word C_fcall lookup(C_word key, int len, C_char *str, C_SYMBOL_TABLE *stable) C_regparm;
static double compute_symbol_table_load(double *avg_bucket_len, int *total);
static C_word C_fcall convert_string_to_number(C_char *str, int radix, C_word *fix, double *flo) C_regparm;
static C_word C_fcall maybe_inexact_to_exact(C_word n) C_regparm;
static void C_fcall mark_system_globals(void) C_regparm;
static void C_fcall remark_system_globals(void) C_regparm;
static void C_fcall really_remark(C_word *x) C_regparm;
static C_word C_fcall intern0(C_char *name) C_regparm;
static void C_fcall update_locative_table(int mode) C_regparm;
static LF_LIST *find_module_handle(C_char *name);
static void set_profile_timer(C_uword freq);
static void take_profile_sample();

static C_cpsproc(call_cc_wrapper) C_noret;
static C_cpsproc(call_cc_values_wrapper) C_noret;
static C_cpsproc(gc_2) C_noret;
static C_cpsproc(allocate_vector_2) C_noret;
static C_cpsproc(generic_trampoline) C_noret;
static void handle_interrupt(void *trampoline) C_noret;
static C_cpsproc(callback_return_continuation) C_noret;
static C_cpsproc(termination_continuation) C_noret;
static C_cpsproc(become_2) C_noret;
static C_cpsproc(copy_closure_2) C_noret;
static C_cpsproc(dump_heap_state_2) C_noret;
static C_cpsproc(sigsegv_trampoline) C_noret;
static C_cpsproc(sigill_trampoline) C_noret;
static C_cpsproc(sigfpe_trampoline) C_noret;
static C_cpsproc(sigbus_trampoline) C_noret;

static C_PTABLE_ENTRY *create_initial_ptable();

#if !defined(NO_DLOAD2) && (defined(HAVE_DLFCN_H) || defined(HAVE_DL_H) || (defined(HAVE_LOADLIBRARY) && defined(HAVE_GETPROCADDRESS)))
static void C_ccall dload_2(C_word, C_word *) C_noret;
#endif


static void
C_dbg(C_char *prefix, C_char *fstr, ...)
{
  va_list va;

  va_start(va, fstr);
#ifdef __ANDROID__
  __android_log_vprint(ANDROID_LOG_DEBUG, prefix, fstr, va);
#else
  C_fflush(C_stdout);
  C_fprintf(C_stderr, "[%s] ", prefix);
  C_vfprintf(C_stderr, fstr, va);
  C_fflush(C_stderr);
#endif
  va_end(va);
}


/* Startup code: */

int CHICKEN_main(int argc, char *argv[], void *toplevel) 
{
  C_word h, s, n;

  if(C_gui_mode) {
#ifdef _WIN32
    parse_argv(GetCommandLine());
    argc = C_main_argc;
    argv = C_main_argv;
#else
    /* ??? */
#endif
  }

  pass_serious_signals = 0;
  CHICKEN_parse_command_line(argc, argv, &h, &s, &n);
  
  if(!CHICKEN_initialize(h, s, n, toplevel))
    panic(C_text("cannot initialize - out of memory"));

  CHICKEN_run(NULL);
  return 0;
}


/* Custom argv parser for Windoze: */

void parse_argv(C_char *cmds)
{
  C_char *ptr = cmds,
         *bptr0, *bptr, *aptr;
  int n = 0;

  C_main_argv = (C_char **)malloc(MAXIMAL_NUMBER_OF_COMMAND_LINE_ARGUMENTS * sizeof(C_char *));

  if(C_main_argv == NULL)
    panic(C_text("cannot allocate argument-list buffer"));

  C_main_argc = 0;

  for(;;) {
    while(isspace((int)(*ptr))) ++ptr;

    if(*ptr == '\0') break;

    for(bptr0 = bptr = buffer; !isspace((int)(*ptr)) && *ptr != '\0'; *(bptr++) = *(ptr++))
      ++n;

    *bptr = '\0';

    aptr = (C_char*) malloc(sizeof(C_char) * (n + 1));
    if (!aptr)
      panic(C_text("cannot allocate argument buffer"));

    C_strlcpy(aptr, bptr0, sizeof(C_char) * (n + 1));

    C_main_argv[ C_main_argc++ ] = aptr;
  }
}


/* Initialize runtime system: */

int CHICKEN_initialize(int heap, int stack, int symbols, void *toplevel)
{
  C_SCHEME_BLOCK *k0;
  int i;
#ifdef HAVE_SIGACTION
  struct sigaction sa;
#endif

  /*FIXME Should have C_tzset in chicken.h? */
#ifdef C_NONUNIX
  C_startup_time_seconds = (time_t)0;
# if defined(__MINGW32__)
  /* Make sure _tzname, _timezone, and _daylight are set */
  _tzset();
# endif
#else
  struct timeval tv;
  C_gettimeofday(&tv, NULL);
  C_startup_time_seconds = tv.tv_sec;
  /* Make sure tzname, timezone, and daylight are set */
  tzset();
#endif

  if(chicken_is_initialized) return 1;
  else chicken_is_initialized = 1;

#if defined(__ANDROID__) && defined(DEBUGBUILD)
  debug_mode = 2;
#endif

  if(debug_mode) 
    C_dbg(C_text("debug"), C_text("application startup...\n"));

  C_panic_hook = usual_panic;
  symbol_table_list = NULL;

  symbol_table = C_new_symbol_table(".", symbols ? symbols : DEFAULT_SYMBOL_TABLE_SIZE);

  if(symbol_table == NULL)
    return 0;

  page_size = 0;
  stack_size = stack ? stack : DEFAULT_STACK_SIZE;
  C_set_or_change_heap_size(heap ? heap : DEFAULT_HEAP_SIZE, 0);

  /* Allocate temporary stack: */
  temporary_stack_size = fixed_temporary_stack_size ? fixed_temporary_stack_size : DEFAULT_TEMPORARY_STACK_SIZE;
  if((C_temporary_stack_limit = (C_word *)C_malloc(temporary_stack_size * sizeof(C_word))) == NULL)
    return 0;
  
  C_temporary_stack_bottom = C_temporary_stack_limit + temporary_stack_size;
  C_temporary_stack = C_temporary_stack_bottom;
  
  /* Allocate mutation stack: */
  mutation_stack_bottom = (C_word **)C_malloc(DEFAULT_MUTATION_STACK_SIZE * sizeof(C_word *));

  if(mutation_stack_bottom == NULL) return 0;

  mutation_stack_top = mutation_stack_bottom;
  mutation_stack_limit = mutation_stack_bottom + DEFAULT_MUTATION_STACK_SIZE;
  C_gc_mutation_hook = NULL;
  C_gc_trace_hook = NULL;

  /* Allocate weak item table: */
  if(C_enable_gcweak) {
    weak_item_table = (WEAK_TABLE_ENTRY *)C_calloc(WEAK_TABLE_SIZE, sizeof(WEAK_TABLE_ENTRY));

    if(weak_item_table == NULL)
      return 0;
  }

  /* Initialize finalizer lists: */
  finalizer_list = NULL;
  finalizer_free_list = NULL;
  pending_finalizer_indices =
      (FINALIZER_NODE **)C_malloc(C_max_pending_finalizers * sizeof(FINALIZER_NODE *));

  if(pending_finalizer_indices == NULL) return 0;

  /* Initialize forwarding table: */
  forwarding_table =
      (C_word *)C_malloc((DEFAULT_FORWARDING_TABLE_SIZE + 1) * 2 * sizeof(C_word));

  if(forwarding_table == NULL) return 0;
  
  *forwarding_table = 0;
  forwarding_table_size = DEFAULT_FORWARDING_TABLE_SIZE;

  /* Initialize locative table: */
  locative_table = (C_word *)C_malloc(DEFAULT_LOCATIVE_TABLE_SIZE * sizeof(C_word));
   
  if(locative_table == NULL) return 0;
 
  locative_table_size = DEFAULT_LOCATIVE_TABLE_SIZE;
  locative_table_count = 0;

  /* Setup collectibles: */
  collectibles = (C_word **)C_malloc(sizeof(C_word *) * DEFAULT_COLLECTIBLES_SIZE);

  if(collectibles == NULL) return 0;

  collectibles_top = collectibles;
  collectibles_limit = collectibles + DEFAULT_COLLECTIBLES_SIZE;
  gc_root_list = NULL;
 
  /* Initialize global variables: */
  if(C_heap_growth <= 0) C_heap_growth = DEFAULT_HEAP_GROWTH;

  if(C_heap_shrinkage <= 0) C_heap_shrinkage = DEFAULT_HEAP_SHRINKAGE;

  if(C_maximal_heap_size <= 0) C_maximal_heap_size = DEFAULT_MAXIMAL_HEAP_SIZE;

#if !defined(NO_DLOAD2) && defined(HAVE_DLFCN_H)
  dlopen_flags = RTLD_LAZY | RTLD_GLOBAL;
#else
  dlopen_flags = 0;
#endif

#ifdef HAVE_SIGACTION
    sa.sa_flags = 0;
    sigfillset(&sa.sa_mask); /* See note in C_establish_signal_handler() */
    sa.sa_handler = global_signal_handler;
#endif

  /* setup signal handlers */
  if(!pass_serious_signals) {
#ifdef HAVE_SIGACTION
    C_sigaction(SIGBUS, &sa, NULL);
    C_sigaction(SIGFPE, &sa, NULL);
    C_sigaction(SIGILL, &sa, NULL);
    C_sigaction(SIGSEGV, &sa, NULL);
#else
    C_signal(SIGBUS, global_signal_handler);
    C_signal(SIGILL, global_signal_handler);
    C_signal(SIGFPE, global_signal_handler);
    C_signal(SIGSEGV, global_signal_handler);
#endif
  }

  tracked_mutation_count = mutation_count = gc_count_1 = gc_count_1_total = gc_count_2 = maximum_heap_usage = 0;
  lf_list = NULL;
  C_register_lf2(NULL, 0, create_initial_ptable());
  C_restart_trampoline = (void *)toplevel;
  trace_buffer = NULL;
  C_clear_trace_buffer();
  chicken_is_running = chicken_ran_once = 0;
  pending_interrupts_count = 0;
  handling_interrupts = 0;
  last_interrupt_latency = 0;
  C_interrupts_enabled = 1;
  C_initial_timer_interrupt_period = INITIAL_TIMER_INTERRUPT_PERIOD;
  C_timer_interrupt_counter = INITIAL_TIMER_INTERRUPT_PERIOD;
  memset(signal_mapping_table, 0, sizeof(int) * NSIG);
  C_dlerror = "cannot load compiled code dynamically - this is a statically linked executable";
  error_location = C_SCHEME_FALSE;
  C_pre_gc_hook = NULL;
  C_post_gc_hook = NULL;
  live_finalizer_count = 0;
  allocated_finalizer_count = 0;
  current_module_name = NULL;
  current_module_handle = NULL;
  callback_continuation_level = 0;
  gc_ms = 0;
  (void)C_randomize(C_fix(time(NULL)));
  initialize_symbol_table();

  if (profiling) {
#ifndef C_NONUNIX
# ifdef HAVE_SIGACTION
    C_sigaction(C_PROFILE_SIGNAL, &sa, NULL);
# else
    C_signal(C_PROFILE_SIGNAL, global_signal_handler);
# endif
#endif

    profile_table = (PROFILE_BUCKET **)C_malloc(PROFILE_TABLE_SIZE * sizeof(PROFILE_BUCKET *));

    if(profile_table == NULL)
      panic(C_text("out of memory - can not allocate profile table"));

    C_memset(profile_table, 0, sizeof(PROFILE_BUCKET *) * PROFILE_TABLE_SIZE);
  }
  
  /* create k to invoke code for system-startup: */
  k0 = (C_SCHEME_BLOCK *)C_align((C_word)C_fromspace_top);
  C_fromspace_top += C_align(2 * sizeof(C_word));
  k0->header = C_CLOSURE_TYPE | 1;
  C_set_block_item(k0, 0, (C_word)termination_continuation);
  C_save(k0);
  C_save(C_SCHEME_UNDEFINED);
  C_restart_c = 2;
  return 1;
}


void *C_get_statistics(void) {
  static void *stats[ 8 ];

  stats[ 0 ] = fromspace_start;
  stats[ 1 ] = C_fromspace_limit;
  stats[ 2 ] = NULL;
  stats[ 3 ] = NULL;
  stats[ 4 ] = C_stack_limit;
  stats[ 5 ] = stack_bottom;
  stats[ 6 ] = C_fromspace_top;
  stats[ 7 ] = NULL;
  return stats;
}


static C_PTABLE_ENTRY *create_initial_ptable()
{
  /* IMPORTANT: hardcoded table size -
     this must match the number of C_pte calls + 1 (NULL terminator)! */
  C_PTABLE_ENTRY *pt = (C_PTABLE_ENTRY *)C_malloc(sizeof(C_PTABLE_ENTRY) * 56);
  int i = 0;

  if(pt == NULL)
    panic(C_text("out of memory - cannot create initial ptable"));

  C_pte(termination_continuation);
  C_pte(callback_return_continuation);
  C_pte(values_continuation);
  C_pte(call_cc_values_wrapper);
  C_pte(call_cc_wrapper);
  C_pte(C_gc);
  C_pte(C_allocate_vector);
  C_pte(C_make_structure);
  C_pte(C_ensure_heap_reserve);
  C_pte(C_return_to_host);
  C_pte(C_get_symbol_table_info);
  C_pte(C_get_memory_info);
  C_pte(C_decode_seconds);
  C_pte(C_stop_timer);
  C_pte(C_dload);
  C_pte(C_set_dlopen_flags);
  C_pte(C_become);
  C_pte(C_apply_values);
  C_pte(C_times);
  C_pte(C_minus);
  C_pte(C_plus);
  C_pte(C_divide);
  C_pte(C_nequalp);
  C_pte(C_greaterp);
  /* IMPORTANT: have you read the comments at the start and the end of this function? */
  C_pte(C_lessp);
  C_pte(C_greater_or_equal_p);
  C_pte(C_less_or_equal_p);
  C_pte(C_quotient);
  C_pte(C_flonum_fraction);
  C_pte(C_flonum_rat);
  C_pte(C_expt);
  C_pte(C_number_to_string);
  C_pte(C_make_symbol);
  C_pte(C_string_to_symbol);
  C_pte(C_apply);
  C_pte(C_call_cc);
  C_pte(C_values);
  C_pte(C_call_with_values);
  C_pte(C_continuation_graft);
  C_pte(C_open_file_port);
  C_pte(C_software_type);
  C_pte(C_machine_type);
  C_pte(C_machine_byte_order);
  C_pte(C_software_version);
  C_pte(C_build_platform);
  C_pte(C_make_pointer);
  C_pte(C_make_tagged_pointer);
  C_pte(C_peek_signed_integer);
  C_pte(C_peek_unsigned_integer);
  C_pte(C_context_switch);
  C_pte(C_register_finalizer);
  C_pte(C_locative_ref); /* OBSOLETE */
  C_pte(C_copy_closure);
  C_pte(C_dump_heap_state);
  C_pte(C_filter_heap_objects);

  /* IMPORTANT: did you remember the hardcoded pte table size? */
  pt[ i ].id = NULL;
  return pt;
}


void *CHICKEN_new_gc_root_2(int finalizable)
{
  C_GC_ROOT *r = (C_GC_ROOT *)C_malloc(sizeof(C_GC_ROOT));

  if(r == NULL)
    panic(C_text("out of memory - cannot allocate GC root"));

  r->value = C_SCHEME_UNDEFINED;
  r->next = gc_root_list;
  r->prev = NULL;
  r->finalizable = finalizable;

  if(gc_root_list != NULL) gc_root_list->prev = r;

  gc_root_list = r;
  return (void *)r;
}


void *CHICKEN_new_gc_root()
{
  return CHICKEN_new_gc_root_2(0);
}


void *CHICKEN_new_finalizable_gc_root()
{
  return CHICKEN_new_gc_root_2(1);
}


void CHICKEN_delete_gc_root(void *root)
{
  C_GC_ROOT *r = (C_GC_ROOT *)root;

  if(r->prev == NULL) gc_root_list = r->next;
  else r->prev->next = r->next;

  if(r->next != NULL) r->next->prev = r->prev;

  C_free(root);
}


void *CHICKEN_global_lookup(char *name)
{
  int 
    len = C_strlen(name),
    key = hash_string(len, name, symbol_table->size, symbol_table->rand, 0);
  C_word s;
  void *root = CHICKEN_new_gc_root();

  if(C_truep(s = lookup(key, len, name, symbol_table))) {
    if(C_block_item(s, 0) != C_SCHEME_UNBOUND) {
      CHICKEN_gc_root_set(root, s);
      return root;
    }
  }

  return NULL;
}


int CHICKEN_is_running()
{
  return chicken_is_running;
}


void CHICKEN_interrupt()
{
  C_timer_interrupt_counter = 0;
}


C_regparm C_SYMBOL_TABLE *C_new_symbol_table(char *name, unsigned int size)
{
  C_SYMBOL_TABLE *stp;
  int i;

  if((stp = C_find_symbol_table(name)) != NULL) return stp;

  if((stp = (C_SYMBOL_TABLE *)C_malloc(sizeof(C_SYMBOL_TABLE))) == NULL)
    return NULL;

  stp->name = name;
  stp->size = size;
  stp->next = symbol_table_list;
  stp->rand = rand();

  if((stp->table = (C_word *)C_malloc(size * sizeof(C_word))) == NULL)
    return NULL;

  for(i = 0; i < stp->size; stp->table[ i++ ] = C_SCHEME_END_OF_LIST);

  symbol_table_list = stp;
  return stp;
}  


C_regparm void C_delete_symbol_table(C_SYMBOL_TABLE *st)
{
  C_SYMBOL_TABLE *stp, *prev = NULL;

  for(stp = symbol_table_list; stp != NULL; stp = stp->next)
    if(stp == st) {
      if(prev != NULL) prev->next = stp->next;
      else symbol_table_list = stp->next;

      return;
    }
}


C_regparm void C_set_symbol_table(C_SYMBOL_TABLE *st)
{
  symbol_table = st;
}


C_regparm C_SYMBOL_TABLE *C_find_symbol_table(char *name)
{
  C_SYMBOL_TABLE *stp;

  for(stp = symbol_table_list; stp != NULL; stp = stp->next)
    if(!C_strcmp(name, stp->name)) return stp;

  return NULL;
}


C_regparm C_word C_find_symbol(C_word str, C_SYMBOL_TABLE *stable)
{
  C_char *sptr = C_c_string(str);
  int len = C_header_size(str);
  int key;
  C_word s;

  if(stable == NULL) stable = symbol_table;

  key = hash_string(len, sptr, stable->size, stable->rand, 0);

  if(C_truep(s = lookup(key, len, sptr, stable))) return s;
  else return C_SCHEME_FALSE;
}


/* Setup symbol-table with internally used symbols; */

void initialize_symbol_table(void)
{
  int i;

  for(i = 0; i < symbol_table->size; symbol_table->table[ i++ ] = C_SCHEME_END_OF_LIST);

  /* Obtain reference to hooks for later: */
  interrupt_hook_symbol = C_intern2(C_heaptop, C_text("\003sysinterrupt-hook"));
  error_hook_symbol = C_intern2(C_heaptop, C_text("\003syserror-hook"));
  callback_continuation_stack_symbol = C_intern3(C_heaptop, C_text("\003syscallback-continuation-stack"), C_SCHEME_END_OF_LIST);
  pending_finalizers_symbol = C_intern2(C_heaptop, C_text("\003syspending-finalizers"));
  current_thread_symbol = C_intern3(C_heaptop, C_text("\003syscurrent-thread"), C_SCHEME_FALSE);
}


void C_ccall sigsegv_trampoline(C_word c, C_word *av)
{
  barf(C_MEMORY_VIOLATION_ERROR, NULL);
}


void C_ccall sigbus_trampoline(C_word c, C_word *av)
{
  barf(C_BUS_ERROR, NULL);
}


void C_ccall sigfpe_trampoline(C_word c, C_word *av)
{
  barf(C_FLOATING_POINT_EXCEPTION_ERROR, NULL);
}


void C_ccall sigill_trampoline(C_word c, C_word *av)
{
  barf(C_ILLEGAL_INSTRUCTION_ERROR, NULL);
}


/* This is called from POSIX signals: */

void global_signal_handler(int signum)
{
#if defined(HAVE_SIGPROCMASK)
  if(signum == SIGSEGV || signum == SIGFPE || signum == SIGILL || signum == SIGBUS) {
    sigset_t sset;
    
    if(serious_signal_occurred || !chicken_is_running) {
      switch(signum) {
      case SIGSEGV: panic(C_text("unrecoverable segmentation violation"));
      case SIGFPE: panic(C_text("unrecoverable floating-point exception"));
      case SIGILL: panic(C_text("unrecoverable illegal instruction error"));
      case SIGBUS: panic(C_text("unrecoverable bus error"));
      default: panic(C_text("unrecoverable serious condition"));
      }
    }
    else serious_signal_occurred = 1;

    /* unblock signal to avoid nested invocation of the handler */
    sigemptyset(&sset);
    sigaddset(&sset, signum);
    C_sigprocmask(SIG_UNBLOCK, &sset, NULL);

    switch(signum) {
    case SIGSEGV: C_reclaim(sigsegv_trampoline, 0);
    case SIGFPE: C_reclaim(sigfpe_trampoline, 0);
    case SIGILL: C_reclaim(sigill_trampoline, 0);
    case SIGBUS: C_reclaim(sigbus_trampoline, 0);
    default: panic(C_text("invalid serious signal"));
    }
  }
#endif

  /* TODO: Make full use of sigaction: check that /our/ timer expired */
  if (signum == C_PROFILE_SIGNAL && profiling) take_profile_sample();
  else C_raise_interrupt(signal_mapping_table[ signum ]);

#ifndef HAVE_SIGACTION
  /* not necessarily needed, but older UNIXen may not leave the handler installed: */
  C_signal(signum, global_signal_handler);
#endif
}


/* Align memory to page boundary */

static void *align_to_page(void *mem)
{
  return (void *)C_align((C_uword)mem);
}


static C_byte *
heap_alloc (size_t size, C_byte **page_aligned)
{
  C_byte *p;
  p = (C_byte *)C_malloc (size + page_size);

  if (p != NULL && page_aligned) *page_aligned = align_to_page (p);

  return p;
}


static void
heap_free (C_byte *ptr, size_t size)
{
  C_free (ptr);
}


static C_byte *
heap_realloc (C_byte *ptr, size_t old_size,
	      size_t new_size, C_byte **page_aligned)
{
  C_byte *p;
  p = (C_byte *)C_realloc (ptr, new_size + page_size);

  if (p != NULL && page_aligned) *page_aligned = align_to_page (p);

  return p;
}


/* Modify heap size at runtime: */

void C_set_or_change_heap_size(C_word heap, int reintern)
{
  C_byte *ptr1, *ptr2, *ptr1a, *ptr2a;
  C_word size = heap / 2;

  if(heap_size_changed && fromspace_start) return;

  if(fromspace_start && heap_size >= heap) return;

  if(debug_mode)
    C_dbg(C_text("debug"), C_text("heap resized to %d bytes\n"), (int)heap);

  heap_size = heap;

  if((ptr1 = heap_realloc (fromspace_start,
			   C_fromspace_limit - fromspace_start,
			   size, &ptr1a)) == NULL ||
     (ptr2 = heap_realloc (tospace_start,
			   tospace_limit - tospace_start,
			   size, &ptr2a)) == NULL)
    panic(C_text("out of memory - cannot allocate heap"));

  heapspace1 = ptr1;
  heapspace1_size = size;
  heapspace2 = ptr2;
  heapspace2_size = size;
  fromspace_start = ptr1a;
  C_fromspace_top = fromspace_start;
  C_fromspace_limit = fromspace_start + size;
  tospace_start = ptr2a;
  tospace_top = tospace_start;
  tospace_limit = tospace_start + size;
  mutation_stack_top = mutation_stack_bottom;

  if(reintern) initialize_symbol_table();
}
 

/* Modify stack-size at runtime: */

void C_do_resize_stack(C_word stack)
{
  C_uword old = stack_size,
          diff = stack - old;

  if(diff != 0 && !stack_size_changed) {
    if(debug_mode) 
      C_dbg(C_text("debug"), C_text("stack resized to %d bytes\n"), (int)stack);

    stack_size = stack;

#if C_STACK_GROWS_DOWNWARD
    C_stack_hard_limit = (C_word *)((C_byte *)C_stack_hard_limit - diff);
#else
    C_stack_hard_limit = (C_word *)((C_byte *)C_stack_hard_limit + diff);
#endif
    C_stack_limit = C_stack_hard_limit;
  }
}


/* Check whether nursery is sufficiently big: */

void C_check_nursery_minimum(C_word words)
{
  if(words >= C_bytestowords(stack_size))
    panic(C_text("nursery is too small - try higher setting using the `-:s' option"));
}

C_word C_resize_pending_finalizers(C_word size) {
  int sz = C_num_to_int(size);

  FINALIZER_NODE **newmem = 
    (FINALIZER_NODE **)C_realloc(pending_finalizer_indices, sz * sizeof(FINALIZER_NODE *));
  
  if (newmem == NULL)
    return C_SCHEME_FALSE;

  pending_finalizer_indices = newmem;
  C_max_pending_finalizers = sz;
  return C_SCHEME_TRUE;
}


/* Parse runtime options from command-line: */

void CHICKEN_parse_command_line(int argc, char *argv[], C_word *heap, C_word *stack, C_word *symbols)
{
  int i;
  char *ptr;
  C_word x;

  C_main_argc = argc;
  C_main_argv = argv;
  *heap = DEFAULT_HEAP_SIZE;
  *stack = DEFAULT_STACK_SIZE;
  *symbols = DEFAULT_SYMBOL_TABLE_SIZE;

  for(i = 1; i < C_main_argc; ++i)
    if(!strncmp(C_main_argv[ i ], C_text("-:"), 2)) {
      for(ptr = &C_main_argv[ i ][ 2 ]; *ptr != '\0';) {
	switch(*(ptr++)) {
	case '?':
	  C_dbg("Runtime options", "\n\n"
		 " -:?              display this text\n"
		 " -:c              always treat stdin as console\n"
		 " -:d              enable debug output\n"
		 " -:D              enable more debug output\n"
		 " -:g              show GC information\n"
		 " -:o              disable stack overflow checks\n"
		 " -:hiSIZE         set initial heap size\n"
		 " -:hmSIZE         set maximal heap size\n"
		 " -:hgPERCENTAGE   set heap growth percentage\n"
		 " -:hsPERCENTAGE   set heap shrink percentage\n"
		 " -:hSIZE          set fixed heap size\n"
		 " -:r              write trace output to stderr\n"
		 " -:p              collect statistical profile and write to file at exit\n"
		 " -:PFREQUENCY     like -:p, specifying sampling frequency in us (default: 10000)\n"
		 " -:sSIZE          set nursery (stack) size\n"
		 " -:tSIZE          set symbol-table size\n"
                 " -:fSIZE          set maximal number of pending finalizers\n"
		 " -:w              enable garbage collection of unused symbols\n"
		 " -:x              deliver uncaught exceptions of other threads to primordial one\n"
		 " -:b              enter REPL on error\n"
		 " -:B              sound bell on major GC\n"
		 " -:G              force GUI mode\n"
		 " -:aSIZE          set trace-buffer/call-chain size\n"
		 " -:ASIZE          set fixed temporary stack size\n"
		 " -:H              dump heap state on exit\n"
		 " -:S              do not handle segfaults or other serious conditions\n"
		 "\n  SIZE may have a `k' (`K'), `m' (`M') or `g' (`G') suffix, meaning size\n"
		 "  times 1024, 1048576, and 1073741824, respectively.\n\n");
	  C_exit_runtime(C_fix(0));

	case 'h':
	  switch(*ptr) {
	  case 'i':
	    *heap = arg_val(ptr + 1); 
	    heap_size_changed = 1;
	    goto next;
	  case 'g':
	    C_heap_growth = arg_val(ptr + 1);
	    goto next;
	  case 'm':
	    C_maximal_heap_size = arg_val(ptr + 1);
	    goto next;
	  case 's':
	    C_heap_shrinkage = arg_val(ptr + 1);
	    goto next;
	  default:
	    *heap = arg_val(ptr); 
	    heap_size_changed = 1;
	    C_heap_size_is_fixed = 1;
	    goto next;
	  }

	case 'o':
	  C_disable_overflow_check = 1;
	  break;

	case 'B':
	  gc_bell = 1;
	  break;

	case 'G':
	  C_gui_mode = 1;
	  break;

	case 'H':
	  dump_heap_on_exit = 1;
	  break;

	case 'S':
	  pass_serious_signals = 1;
	  break;

	case 's':
	  *stack = arg_val(ptr);
	  stack_size_changed = 1;
	  goto next;

	case 'f':
	  C_max_pending_finalizers = arg_val(ptr);
	  goto next;

	case 'a':
	  C_trace_buffer_size = arg_val(ptr);
	  goto next;

	case 'A':
	  fixed_temporary_stack_size = arg_val(ptr);
	  goto next;

	case 't':
	  *symbols = arg_val(ptr);
	  goto next;

	case 'c':
	  fake_tty_flag = 1;
	  break;

	case 'd':
	  debug_mode = 1;
	  break;

	case 'D':
	  debug_mode = 2;
	  break;

	case 'g':
	  gc_report_flag = 2;
	  break;

	case 'w':
	  C_enable_gcweak = 1;
	  break;

	case 'P':
	  profiling = 1;
	  profile_frequency = arg_val(ptr);
          goto next;

	case 'p':
	  profiling = 1;
          break;

	case 'r':
	  show_trace = 1;
	  break;

	case 'x':
	  C_abort_on_thread_exceptions = 1;
	  break;

	case 'b':
	  C_enable_repl = 1;
	  break;

	default: panic(C_text("illegal runtime option"));
	}
      }

    next:;
    }
}


C_word arg_val(C_char *arg)
{
  int len;
  C_char *end;
  C_long val, mul = 1;

  if (arg == NULL) panic(C_text("illegal runtime-option argument"));
      
  len = C_strlen(arg);
      
  if(len < 1) panic(C_text("illegal runtime-option argument"));
      
  switch(arg[ len - 1 ]) {
  case 'k':
  case 'K': mul = 1024; break;
 	  
  case 'm':
  case 'M': mul = 1024 * 1024; break;
 	  
  case 'g':
  case 'G': mul = 1024 * 1024 * 1024; break;

  default: mul = 1;
  }

  val = C_strtow(arg, &end, 10);

  if((mul != 1 ? end[ 1 ] != '\0' : end[ 0 ] != '\0')) 
    panic(C_text("invalid runtime-option argument suffix"));

  return val * mul;
}


/* Run embedded code with arguments: */

C_word CHICKEN_run(void *toplevel)
{
  if(!chicken_is_initialized && !CHICKEN_initialize(0, 0, 0, toplevel))
    panic(C_text("could not initialize"));

  if(chicken_is_running)
    panic(C_text("re-invocation of Scheme world while process is already running"));

  chicken_is_running = chicken_ran_once = 1;
  return_to_host = 0;

  if(profiling) set_profile_timer(profile_frequency);

#if C_STACK_GROWS_DOWNWARD
  C_stack_hard_limit = (C_word *)((C_byte *)C_stack_pointer - stack_size);
#else
  C_stack_hard_limit = (C_word *)((C_byte *)C_stack_pointer + stack_size);
#endif
  C_stack_limit = C_stack_hard_limit;

  stack_bottom = C_stack_pointer;

  if(debug_mode)
    C_dbg(C_text("debug"), C_text("stack bottom is 0x%lx.\n"), (C_word)stack_bottom);

  /* The point of (usually) no return... */
#ifdef HAVE_SIGSETJMP
  C_sigsetjmp(C_restart, 0);
#else
  C_setjmp(C_restart);
#endif

  serious_signal_occurred = 0;

  if(!return_to_host) {
    /* We must copy the argvector onto the stack, because
     * any subsequent save() will otherwise clobber it.
     */
    C_word *p = C_alloc(C_restart_c);
    assert(C_restart_c == (C_temporary_stack_bottom - C_temporary_stack));
    C_memcpy(p, C_temporary_stack, C_restart_c * sizeof(C_word));
    C_temporary_stack = C_temporary_stack_bottom;
    ((C_proc)C_restart_trampoline)(C_restart_c, p);
  }

  if(profiling) set_profile_timer(0);

  chicken_is_running = 0;
  return C_restore;
}


C_word CHICKEN_continue(C_word k)
{
  if(C_temporary_stack_bottom != C_temporary_stack)
    panic(C_text("invalid temporary stack level"));

  if(!chicken_is_initialized)
    panic(C_text("runtime system has not been initialized - `CHICKEN_run' has probably not been called"));

  C_save(k);
  return CHICKEN_run(NULL);
}


/* The final continuation: */

void C_ccall termination_continuation(C_word c, C_word *av)
{
  if(debug_mode) {
    C_dbg(C_text("debug"), C_text("application terminated normally\n"));
  }

  C_exit_runtime(C_fix(0));
}


/* Signal unrecoverable runtime error: */

void panic(C_char *msg)
{
  if(C_panic_hook != NULL) C_panic_hook(msg);

  usual_panic(msg);
}


void usual_panic(C_char *msg)
{
  C_char *dmp = C_dump_trace(0);

  C_dbg_hook(C_SCHEME_UNDEFINED);

  if(C_gui_mode) {
    C_snprintf(buffer, sizeof(buffer), C_text("%s\n\n%s"), msg, dmp);
#if defined(_WIN32) && !defined(__CYGWIN__)
    MessageBox(NULL, buffer, C_text("CHICKEN runtime"), MB_OK | MB_ICONERROR);
    ExitProcess(1);
#endif
  } /* fall through if not WIN32 GUI app */

  C_dbg("panic", C_text("%s - execution terminated\n\n%s"), msg, dmp);
  C_exit_runtime(C_fix(1));
}


void horror(C_char *msg)
{
  C_dbg_hook(C_SCHEME_UNDEFINED);

  if(C_gui_mode) {
    C_snprintf(buffer, sizeof(buffer), C_text("%s"), msg);
#if defined(_WIN32) && !defined(__CYGWIN__)
    MessageBox(NULL, buffer, C_text("CHICKEN runtime"), MB_OK | MB_ICONERROR);
    ExitProcess(1);
#endif
  } /* fall through */

  C_dbg("horror", C_text("\n%s - execution terminated"), msg);  
  C_exit_runtime(C_fix(1));
}


/* Error-hook, called from C-level runtime routines: */

void barf(int code, char *loc, ...)
{
  C_char *msg;
  C_word err = error_hook_symbol;
  int c, i;
  va_list v;
  C_word *av; 

  C_dbg_hook(C_SCHEME_UNDEFINED);

  C_temporary_stack = C_temporary_stack_bottom;
  err = C_block_item(err, 0);

  if(C_immediatep(err))
    panic(C_text("`##sys#error-hook' is not defined - the `library' unit was probably not linked with this executable"));

  switch(code) {
  case C_BAD_ARGUMENT_COUNT_ERROR:
    msg = C_text("bad argument count");
    c = 3;
    break;

  case C_BAD_MINIMUM_ARGUMENT_COUNT_ERROR:
    msg = C_text("too few arguments");
    c = 3;
    break;
  
  case C_BAD_ARGUMENT_TYPE_ERROR:
    msg = C_text("bad argument type");
    c = 1;
    break;

  case C_UNBOUND_VARIABLE_ERROR:
    msg = C_text("unbound variable");
    c = 1;
    break;

  case C_OUT_OF_MEMORY_ERROR:
    msg = C_text("not enough memory");
    c = 0;
    break;

  case C_DIVISION_BY_ZERO_ERROR:
    msg = C_text("division by zero");
    c = 0;
    break;

  case C_OUT_OF_RANGE_ERROR:
    msg = C_text("out of range");
    c = 2;
    break;

  case C_NOT_A_CLOSURE_ERROR:
    msg = C_text("call of non-procedure");
    c = 1;
    break;

  case C_CONTINUATION_CANT_RECEIVE_VALUES_ERROR:
    msg = C_text("continuation cannot receive multiple values");
    c = 1;
    break;

  case C_BAD_ARGUMENT_TYPE_CYCLIC_LIST_ERROR:
    msg = C_text("bad argument type - not a non-cyclic list");
    c = 1;
    break;

  case C_TOO_DEEP_RECURSION_ERROR:
    msg = C_text("recursion too deep");
    c = 0;
    break;

  case C_CANT_REPRESENT_INEXACT_ERROR:
    msg = C_text("inexact number cannot be represented as an exact number");
    c = 1;
    break;

  case C_NOT_A_PROPER_LIST_ERROR:
    msg = C_text("bad argument type - not a proper list");
    c = 1;
    break;

  case C_BAD_ARGUMENT_TYPE_NO_FIXNUM_ERROR:
    msg = C_text("bad argument type - not a fixnum");
    c = 1;
    break;

  case C_BAD_ARGUMENT_TYPE_NO_STRING_ERROR:
    msg = C_text("bad argument type - not a string");
    c = 1;
    break;

  case C_BAD_ARGUMENT_TYPE_NO_PAIR_ERROR:
    msg = C_text("bad argument type - not a pair");
    c = 1;
    break;

  case C_BAD_ARGUMENT_TYPE_NO_BOOLEAN_ERROR:
    msg = C_text("bad argument type - not a boolean");
    c = 1;
    break;

  case C_BAD_ARGUMENT_TYPE_NO_LOCATIVE_ERROR:
    msg = C_text("bad argument type - not a locative");
    c = 1;
    break;

  case C_BAD_ARGUMENT_TYPE_NO_LIST_ERROR:
    msg = C_text("bad argument type - not a list");
    c = 1;
    break;

  case C_BAD_ARGUMENT_TYPE_NO_NUMBER_ERROR:
    msg = C_text("bad argument type - not a number");
    c = 1;
    break;

  case C_BAD_ARGUMENT_TYPE_NO_SYMBOL_ERROR:
    msg = C_text("bad argument type - not a symbol");
    c = 1;
    break;

  case C_BAD_ARGUMENT_TYPE_NO_VECTOR_ERROR:
    msg = C_text("bad argument type - not a vector");
    c = 1;
    break;

  case C_BAD_ARGUMENT_TYPE_NO_CHAR_ERROR:
    msg = C_text("bad argument type - not a character");
    c = 1;
    break;

  case C_STACK_OVERFLOW_ERROR:
    msg = C_text("stack overflow");
    c = 0;
    break;

  case C_BAD_ARGUMENT_TYPE_BAD_STRUCT_ERROR:
    msg = C_text("bad argument type - not a structure of the required type");
    c = 2;
    break;

  case C_BAD_ARGUMENT_TYPE_NO_BYTEVECTOR_ERROR:
    msg = C_text("bad argument type - not a blob");
    c = 1;
    break;

  case C_LOST_LOCATIVE_ERROR:
    msg = C_text("locative refers to reclaimed object");
    c = 1;
    break;

  case C_BAD_ARGUMENT_TYPE_NO_BLOCK_ERROR:
    msg = C_text("bad argument type - not a non-immediate value");
    c = 1;
    break;

  case C_BAD_ARGUMENT_TYPE_NO_NUMBER_VECTOR_ERROR:
    msg = C_text("bad argument type - not a number vector");
    c = 2;
    break;

  case C_BAD_ARGUMENT_TYPE_NO_INTEGER_ERROR:
    msg = C_text("bad argument type - not an integer");
    c = 1;
    break;

  case C_BAD_ARGUMENT_TYPE_NO_UINTEGER_ERROR:
    msg = C_text("bad argument type - not an unsigned integer");
    c = 1;
    break;

  case C_BAD_ARGUMENT_TYPE_NO_POINTER_ERROR:
    msg = C_text("bad argument type - not a pointer");
    c = 1;
    break;

  case C_BAD_ARGUMENT_TYPE_NO_TAGGED_POINTER_ERROR:
    msg = C_text("bad argument type - not a tagged pointer");
    c = 2;
    break;

  case C_BAD_ARGUMENT_TYPE_NO_FLONUM_ERROR:
    msg = C_text("bad argument type - not a flonum");
    c = 1;
    break;

  case C_BAD_ARGUMENT_TYPE_NO_CLOSURE_ERROR:
    msg = C_text("bad argument type - not a procedure");
    c = 1;
    break;

  case C_BAD_ARGUMENT_TYPE_BAD_BASE_ERROR:
    msg = C_text("bad argument type - invalid base");
    c = 1;
    break;

  case C_CIRCULAR_DATA_ERROR:
    msg = C_text("recursion too deep or circular data encountered");
    c = 0;
    break;

  case C_BAD_ARGUMENT_TYPE_NO_PORT_ERROR:
    msg = C_text("bad argument type - not a port");
    c = 1;
    break;

  case C_BAD_ARGUMENT_TYPE_NO_INPUT_PORT_ERROR:
    msg = C_text("bad argument type - not an input-port");
    c = 1;
    break;

  case C_BAD_ARGUMENT_TYPE_NO_OUTPUT_PORT_ERROR:
    msg = C_text("bad argument type - not an output-port");
    c = 1;
    break;

  case C_PORT_CLOSED_ERROR:
    msg = C_text("port already closed");
    c = 1;
    break;
 
  case C_ASCIIZ_REPRESENTATION_ERROR:
    msg = C_text("cannot represent string with NUL bytes as C string");
    c = 1;
    break;

  case C_MEMORY_VIOLATION_ERROR:
    msg = C_text("segmentation violation");
    c = 0;
    break;

  case C_FLOATING_POINT_EXCEPTION_ERROR:
    msg = C_text("floating point exception");
    c = 0;
    break;

  case C_ILLEGAL_INSTRUCTION_ERROR:
    msg = C_text("illegal instruction");
    c = 0;
    break;

  case C_BUS_ERROR:
    msg = C_text("bus error");
    c = 0;
    break;

  default: panic(C_text("illegal internal error code"));
  }

  av = C_alloc(c + 4);
  
  if(!C_immediatep(err)) {
    va_start(v, loc);
    av[ 0 ] = err;
    /* No continuation is passed: '##sys#error-hook' may not return: */
    av[ 1 ] = C_SCHEME_UNDEFINED;
    av[ 2 ] = C_fix(code);
    
    if(loc != NULL)
      av[ 3 ] = intern0(loc);
    else {
      av[ 3 ] = error_location;
      error_location = C_SCHEME_FALSE;
    }

    for(i = 0; i < c; ++i)
      av[ i + 4 ] = va_arg(v, C_word);

    va_end(v);
    C_do_apply(c + 4, av);
  }
  else panic(msg);
}


/* Hook for setting breakpoints */

C_word C_dbg_hook(C_word dummy)
{
  return dummy;
}


/* Timing routines: */

C_regparm double C_fcall C_milliseconds(void)
{
#ifdef C_NONUNIX
    if(CLOCKS_PER_SEC == 1000) return clock();
    else return C_floor(((double)clock() / (double)CLOCKS_PER_SEC) * 1000);
#else
    struct timeval tv;

    if(C_gettimeofday(&tv, NULL) == -1) return 0;
    else return C_floor(((double)tv.tv_sec - C_startup_time_seconds) * 1000.0 + tv.tv_usec / 1000);
#endif
}


C_regparm time_t C_fcall C_seconds(C_long *ms)
{
#ifdef C_NONUNIX
  if(ms != NULL) *ms = 0;

  return (time_t)(clock() / CLOCKS_PER_SEC);
#else
  struct timeval tv;

  if(C_gettimeofday(&tv, NULL) == -1) {
    if(ms != NULL) *ms = 0;

    return (time_t)0;
  }
  else {
    if(ms != NULL) *ms = tv.tv_usec / 1000;

    return tv.tv_sec;
  }
#endif
}


C_regparm double C_fcall C_cpu_milliseconds(void)
{
#if defined(C_NONUNIX) || defined(__CYGWIN__)
    if(CLOCKS_PER_SEC == 1000) return clock();
    else return C_floor(((double)clock() / (double)CLOCKS_PER_SEC) * 1000);
#else
    struct rusage ru;

    if(C_getrusage(RUSAGE_SELF, &ru) == -1) return 0;
    else return 
	   C_floor(((double)ru.ru_utime.tv_sec + ru.ru_stime.tv_sec) * 1000
		   + ((double)ru.ru_utime.tv_usec + ru.ru_stime.tv_usec) / 1000);
#endif
}


/* Support code for callbacks: */

int C_fcall C_save_callback_continuation(C_word **ptr, C_word k)
{
  C_word p = C_a_pair(ptr, k, C_block_item(callback_continuation_stack_symbol, 0));
  
  C_mutate_slot(&C_block_item(callback_continuation_stack_symbol, 0), p);
  return ++callback_continuation_level;
}


C_word C_fcall C_restore_callback_continuation(void) 
{
  /* obsolete, but retained for keeping old code working */
  C_word p = C_block_item(callback_continuation_stack_symbol, 0),
         k;

  assert(!C_immediatep(p) && C_block_header(p) == C_PAIR_TAG);
  k = C_u_i_car(p);

  C_mutate2(&C_block_item(callback_continuation_stack_symbol, 0), C_u_i_cdr(p));
  --callback_continuation_level;
  return k;
}


C_word C_fcall C_restore_callback_continuation2(int level) 
{
  C_word p = C_block_item(callback_continuation_stack_symbol, 0),
         k;

  if(level != callback_continuation_level || C_immediatep(p) || C_block_header(p) != C_PAIR_TAG)
    panic(C_text("unbalanced callback continuation stack"));

  k = C_u_i_car(p);

  C_mutate2(&C_block_item(callback_continuation_stack_symbol, 0), C_u_i_cdr(p));
  --callback_continuation_level;
  return k;
}


C_word C_fcall C_callback(C_word closure, int argc)
{
#ifdef HAVE_SIGSETJMP
  sigjmp_buf prev;
#else
  jmp_buf prev;
#endif
  C_word 
    *a = C_alloc(C_SIZEOF_CLOSURE(2)),
    k = C_closure(&a, 2, (C_word)callback_return_continuation, C_SCHEME_FALSE),
    *av;
  int old = chicken_is_running;

  if(old && C_block_item(callback_continuation_stack_symbol, 0) == C_SCHEME_END_OF_LIST)
    panic(C_text("callback invoked in non-safe context"));

  C_memcpy(&prev, &C_restart, sizeof(C_restart));
  callback_returned_flag = 0;       
  chicken_is_running = 1;
  av = C_alloc(argc + 2);
  av[ 0 ] = closure;
  av[ 1 ] = k;
  /*XXX is the order of arguments an issue? */
  C_memcpy(av + 2, C_temporary_stack, argc * sizeof(C_word));
  C_temporary_stack = C_temporary_stack_bottom;
  
#ifdef HAVE_SIGSETJMP
  if(!C_sigsetjmp(C_restart, 0)) C_do_apply(argc + 2, av);
#else
  if(!C_setjmp(C_restart)) C_do_apply(argc + 2, av);
#endif

  serious_signal_occurred = 0;

  if(!callback_returned_flag) {
    /* We must copy the argvector onto the stack, because
     * any subsequent save() will otherwise clobber it.
     */
    C_word *p = C_alloc(C_restart_c);
    assert(C_restart_c == (C_temporary_stack_bottom - C_temporary_stack));
    C_memcpy(p, C_temporary_stack, C_restart_c * sizeof(C_word));
    C_temporary_stack = C_temporary_stack_bottom;
    ((C_proc)C_restart_trampoline)(C_restart_c, p);
  }
  else {
    C_memcpy(&C_restart, &prev, sizeof(C_restart));
    callback_returned_flag = 0;
  }
 
  chicken_is_running = old;
  return C_restore;
}


void C_fcall C_callback_adjust_stack(C_word *a, int size)
{
  if(!chicken_is_running && !C_in_stackp((C_word)a)) {
    if(debug_mode)
      C_dbg(C_text("debug"), 
	    C_text("callback invoked in lower stack region - adjusting limits:\n"
		   "[debug]   current:  \t%p\n"
		   "[debug]   previous: \t%p (bottom) - %p (limit)\n"),
	    a, stack_bottom, C_stack_limit);

#if C_STACK_GROWS_DOWNWARD
    C_stack_hard_limit = (C_word *)((C_byte *)a - stack_size);
    stack_bottom = a + size;
#else
    C_stack_hard_limit = (C_word *)((C_byte *)a + stack_size);
    stack_bottom = a;
#endif
    C_stack_limit = C_stack_hard_limit;

    if(debug_mode)
      C_dbg(C_text("debug"), C_text("new:      \t%p (bottom) - %p (limit)\n"),
	    stack_bottom, C_stack_limit);
  }
}


C_word C_fcall C_callback_wrapper(void *proc, int argc)
{
  C_word
    *a = C_alloc(2),
    closure = C_closure(&a, 1, (C_word)proc),
    result;

  result = C_callback(closure, argc);
  assert(C_temporary_stack == C_temporary_stack_bottom);
  return result;
}


void C_ccall callback_return_continuation(C_word c, C_word *av)
{
  C_word self = av[0];
  C_word r = av[1];

  if(C_block_item(self, 1) == C_SCHEME_TRUE)
    panic(C_text("callback returned twice"));

  assert(callback_returned_flag == 0);
  callback_returned_flag = 1;
  C_set_block_item(self, 1, C_SCHEME_TRUE);
  C_save(r);
  C_reclaim(NULL, 0);
}


/* Register/unregister literal frame: */

void C_initialize_lf(C_word *lf, int count)
{
  while(count-- > 0)
    *(lf++) = C_SCHEME_UNBOUND;
}


void *C_register_lf(C_word *lf, int count)
{
  return C_register_lf2(lf, count, NULL);
}


void *C_register_lf2(C_word *lf, int count, C_PTABLE_ENTRY *ptable)
{
  LF_LIST *node = (LF_LIST *)C_malloc(sizeof(LF_LIST));
  LF_LIST *np;
  int status = 0;

  node->lf = lf;
  node->count = count;
  node->ptable = ptable;
  node->module_name = current_module_name;
  node->module_handle = current_module_handle;
  current_module_handle = NULL;

  if(lf_list) lf_list->prev = node;

  node->next = lf_list;
  node->prev = NULL;
  lf_list = node;
  return (void *)node;
}


LF_LIST *find_module_handle(char *name)
{
  LF_LIST *np;

  for(np = lf_list; np != NULL; np = np->next) {
    if(np->module_name != NULL && !C_strcmp(np->module_name, name)) 
      return np;
  }

  return NULL;
}


void C_unregister_lf(void *handle)
{
  LF_LIST *node = (LF_LIST *) handle;

  if (node->next) node->next->prev = node->prev;

  if (node->prev) node->prev->next = node->next;

  if (lf_list == node) lf_list = node->next;

  C_free(node->module_name);
  C_free(node);
}


/* Intern symbol into symbol-table: */

C_regparm C_word C_fcall C_intern(C_word **ptr, int len, C_char *str) 
{
  return C_intern_in(ptr, len, str, symbol_table);
}


C_regparm C_word C_fcall C_h_intern(C_word *slot, int len, C_char *str)
{
  return C_h_intern_in(slot, len, str, symbol_table);
}


C_regparm C_word C_fcall C_intern_in(C_word **ptr, int len, C_char *str, C_SYMBOL_TABLE *stable)
{
  int key;
  C_word s;

  if(stable == NULL) stable = symbol_table;

  key = hash_string(len, str, stable->size, stable->rand, 0);

  if(C_truep(s = lookup(key, len, str, stable))) return s;

  s = C_string(ptr, len, str);
  return add_symbol(ptr, key, s, stable);
}


C_regparm C_word C_fcall C_h_intern_in(C_word *slot, int len, C_char *str, C_SYMBOL_TABLE *stable)
{
  /* Intern as usual, but remember slot, if looked up symbol is in nursery.
     also: allocate in static memory. */
  int key;
  C_word s;

  if(stable == NULL) stable = symbol_table;

  key = hash_string(len, str, stable->size, stable->rand, 0);

  if(C_truep(s = lookup(key, len, str, stable))) {
    if(C_in_stackp(s)) C_mutate_slot(slot, s);
    
    return s;
  }

  s = C_static_string(C_heaptop, len, str);
  return add_symbol(C_heaptop, key, s, stable);
}


C_regparm C_word C_fcall intern0(C_char *str)
{
  int len = C_strlen(str);
  int key = hash_string(len, str, symbol_table->size, symbol_table->rand, 0);
  C_word s;

  if(C_truep(s = lookup(key, len, str, symbol_table))) return s;
  else return C_SCHEME_FALSE;
}


C_regparm C_word C_fcall C_lookup_symbol(C_word sym)
{
  int key;
  C_word str = C_block_item(sym, 1);
  int len = C_header_size(str);

  key = hash_string(len, C_c_string(str), symbol_table->size, symbol_table->rand, 0);

  return lookup(key, len, C_c_string(str), symbol_table);
}


C_regparm C_word C_fcall C_intern2(C_word **ptr, C_char *str)
{
  return C_intern_in(ptr, C_strlen(str), str, symbol_table);
}


C_regparm C_word C_fcall C_intern3(C_word **ptr, C_char *str, C_word value)
{
  C_word s = C_intern_in(ptr, C_strlen(str), str, symbol_table);
  
  C_mutate2(&C_block_item(s,0), value);
  return s;
}


C_regparm C_word C_fcall hash_string(int len, C_char *str, C_word m, C_word r, int ci)
{
  C_uword key = r;

  if (ci)
    while(len--) key ^= (key << 6) + (key >> 2) + C_tolower((int)(*str++));
  else
    while(len--) key ^= (key << 6) + (key >> 2) + *(str++);

  return (C_word)(key % (C_uword)m);
}


C_regparm C_word C_fcall lookup(C_word key, int len, C_char *str, C_SYMBOL_TABLE *stable)
{
  C_word bucket, sym, s;

  for(bucket = stable->table[ key ]; bucket != C_SCHEME_END_OF_LIST; 
      bucket = C_block_item(bucket,1)) {
    sym = C_block_item(bucket,0);
    s = C_block_item(sym, 1);

    if(C_header_size(s) == (C_word)len
       && !C_memcmp(str, (C_char *)C_data_pointer(s), len))
      return sym;
  }

  return C_SCHEME_FALSE;
}


double compute_symbol_table_load(double *avg_bucket_len, int *total_n)
{
  C_word bucket;
  int i, j, alen = 0, bcount = 0, total = 0;

  for(i = 0; i < symbol_table->size; ++i) {
    bucket = symbol_table->table[ i ];

    for(j = 0; bucket != C_SCHEME_END_OF_LIST; ++j)
      bucket = C_block_item(bucket,1);

    if(j > 0) {
      alen += j;
      ++bcount;
    }

    total += j;
  }

  if(avg_bucket_len != NULL)
    *avg_bucket_len = (double)alen / (double)bcount;

  *total_n = total;

  /* return load: */
  return (double)total / (double)symbol_table->size;
}


C_word add_symbol(C_word **ptr, C_word key, C_word string, C_SYMBOL_TABLE *stable)
{
  C_word bucket, sym, b2, *p;
  int keyw = C_header_size(string) > 0 && *((char *)C_data_pointer(string)) == 0;

  p = *ptr;
  sym = (C_word)p;
  p += C_SIZEOF_SYMBOL;
  C_block_header_init(sym, C_SYMBOL_TYPE | (C_SIZEOF_SYMBOL - 1));
  C_set_block_item(sym, 0, keyw ? sym : C_SCHEME_UNBOUND); /* keyword? */
  C_set_block_item(sym, 1, string);
  C_set_block_item(sym, 2, C_SCHEME_END_OF_LIST);
  *ptr = p;
  b2 = stable->table[ key ];	/* previous bucket */
  bucket = C_a_bucket(ptr, sym, b2); /* create new bucket */

  if(ptr != C_heaptop) C_mutate_slot(&stable->table[ key ], bucket);
  else {
    /* If a stack-allocated bucket was here, and we allocate from 
       heap-top (say, in a toplevel literal frame allocation) then we have
       to inform the memory manager that a 2nd gen. block points to a 
       1st gen. block, hence the mutation: */
    C_mutate2(&C_block_item(bucket,1), b2);
    stable->table[ key ] = bucket;
  }

  return sym;
}


C_regparm int C_in_stackp(C_word x)
{
  C_word *ptr = (C_word *)(C_uword)x;

#if C_STACK_GROWS_DOWNWARD
  return ptr >= C_stack_pointer_test && ptr <= stack_bottom;
#else
  return ptr < C_stack_pointer_test && ptr >= stack_bottom;
#endif
}


C_regparm int C_fcall C_in_heapp(C_word x)
{
  C_byte *ptr = (C_byte *)(C_uword)x;
  return (ptr >= fromspace_start && ptr < C_fromspace_limit) ||
         (ptr >= tospace_start && ptr < tospace_limit);
}

/* Only used during major GC (heap realloc) */
static C_regparm int C_fcall C_in_new_heapp(C_word x)
{
  C_byte *ptr = (C_byte *)(C_uword)x;
  return (ptr >= new_tospace_start && ptr < new_tospace_limit);
}

C_regparm int C_fcall C_in_fromspacep(C_word x)
{
  C_byte *ptr = (C_byte *)(C_uword)x;
  return (ptr >= fromspace_start && ptr < C_fromspace_limit);
}


/* Cons the rest-aguments together: */

C_regparm C_word C_fcall C_build_rest(C_word **ptr, C_word c, C_word n, C_word *av)
{
  C_word
    x = C_SCHEME_END_OF_LIST,
    *p = *ptr;
  C_SCHEME_BLOCK *node;

  av += c;

  while(--c >= n) {
    node = (C_SCHEME_BLOCK *)p;
    p += 3;
    node->header = C_PAIR_TYPE | (C_SIZEOF_PAIR - 1);
    node->data[ 0 ] = *(--av);
    node->data[ 1 ] = x;
    x = (C_word)node;
  }

  *ptr = p;
  return x;
}


/* Print error messages and exit: */

void C_bad_memory(void)
{
  panic(C_text("there is not enough stack-space to run this executable"));
}


void C_bad_memory_2(void)
{
  panic(C_text("there is not enough heap-space to run this executable - try using the '-:h...' option"));
}


/* The following two can be thrown out in the next release... */

void C_bad_argc(int c, int n)
{
  C_bad_argc_2(c, n, C_SCHEME_FALSE);
}


void C_bad_min_argc(int c, int n)
{
  C_bad_min_argc_2(c, n, C_SCHEME_FALSE);
}


void C_bad_argc_2(int c, int n, C_word closure)
{
  barf(C_BAD_ARGUMENT_COUNT_ERROR, NULL, C_fix(n - 2), C_fix(c - 2), closure);
}


void C_bad_min_argc_2(int c, int n, C_word closure)
{
  barf(C_BAD_MINIMUM_ARGUMENT_COUNT_ERROR, NULL, C_fix(n - 2), C_fix(c - 2), closure);
}


void C_stack_overflow(void)
{
  barf(C_STACK_OVERFLOW_ERROR, NULL);
}


void C_stack_overflow_with_loc(C_char *loc)
{
  barf(C_STACK_OVERFLOW_ERROR, loc);
}

void C_unbound_error(C_word sym)
{
  barf(C_UNBOUND_VARIABLE_ERROR, NULL, sym);
}


void C_no_closure_error(C_word x)
{
  barf(C_NOT_A_CLOSURE_ERROR, NULL, x);
}


void C_div_by_zero_error(char *loc)
{
  barf(C_DIVISION_BY_ZERO_ERROR, loc);
}


/* Allocate and initialize record: */

C_regparm C_word C_fcall C_string(C_word **ptr, int len, C_char *str)
{
  C_word strblock = (C_word)(*ptr);

  *ptr = (C_word *)((C_word)(*ptr) + sizeof(C_header) + C_align(len));
  C_block_header_init(strblock, C_STRING_TYPE | len);
  C_memcpy(C_data_pointer(strblock), str, len);
  return strblock;
}


C_regparm C_word C_fcall C_static_string(C_word **ptr, int len, C_char *str)
{
  C_word *dptr = (C_word *)C_malloc(sizeof(C_header) + C_align(len));
  C_word strblock;

  if(dptr == NULL)
    panic(C_text("out of memory - cannot allocate static string"));
    
  strblock = (C_word)dptr;
  C_block_header_init(strblock, C_STRING_TYPE | len);
  C_memcpy(C_data_pointer(strblock), str, len);
  return strblock;
}


C_regparm C_word C_fcall C_static_lambda_info(C_word **ptr, int len, C_char *str)
{
  int dlen = sizeof(C_header) + C_align(len);
  void *dptr = C_malloc(dlen);
  C_word strblock;

  if(dptr == NULL)
    panic(C_text("out of memory - cannot allocate static lambda info"));

  strblock = (C_word)dptr;
  C_block_header_init(strblock, C_LAMBDA_INFO_TYPE | len);
  C_memcpy(C_data_pointer(strblock), str, len);
  return strblock;
}


C_regparm C_word C_fcall C_bytevector(C_word **ptr, int len, C_char *str)
{
  C_word strblock = C_string(ptr, len, str);

  (void)C_string_to_bytevector(strblock);
  return strblock;
}


C_regparm C_word C_fcall C_static_bytevector(C_word **ptr, int len, C_char *str)
{
  C_word strblock = C_static_string(ptr, len, str);

  C_block_header_init(strblock, C_BYTEVECTOR_TYPE | len);
  return strblock;
}


C_regparm C_word C_fcall C_pbytevector(int len, C_char *str)
{
  C_SCHEME_BLOCK *pbv = C_malloc(len + sizeof(C_header));

  if(pbv == NULL) panic(C_text("out of memory - cannot allocate permanent blob"));

  pbv->header = C_BYTEVECTOR_TYPE | len;
  C_memcpy(pbv->data, str, len);
  return (C_word)pbv;
}


C_regparm C_word C_fcall C_string_aligned8(C_word **ptr, int len, C_char *str)
{
  C_word *p = *ptr,
         *p0;

#ifndef C_SIXTY_FOUR
  /* Align on 8-byte boundary: */
  if(C_aligned8(p)) ++p;
#endif

  p0 = p;
  *ptr = p + 1 + C_bytestowords(len);
  *(p++) = C_STRING_TYPE | C_8ALIGN_BIT | len;
  C_memcpy(p, str, len);
  return (C_word)p0;
}


C_regparm C_word C_fcall C_string2(C_word **ptr, C_char *str)
{
  C_word strblock = (C_word)(*ptr);
  int len;

  if(str == NULL) return C_SCHEME_FALSE;

  len = C_strlen(str);
  *ptr = (C_word *)((C_word)(*ptr) + sizeof(C_header) + C_align(len));
  C_block_header_init(strblock, C_STRING_TYPE | len);
  C_memcpy(C_data_pointer(strblock), str, len);
  return strblock;
}


C_regparm C_word C_fcall C_string2_safe(C_word **ptr, int max, C_char *str)
{
  C_word strblock = (C_word)(*ptr);
  int len;

  if(str == NULL) return C_SCHEME_FALSE;

  len = C_strlen(str);

  if(len >= max) {
    C_snprintf(buffer, sizeof(buffer), C_text("foreign string result exceeded maximum of %d bytes"), max);
    panic(buffer);
  }

  *ptr = (C_word *)((C_word)(*ptr) + sizeof(C_header) + C_align(len));
  C_block_header_init(strblock, C_STRING_TYPE | len);
  C_memcpy(C_data_pointer(strblock), str, len);
  return strblock;
}


C_word C_fcall C_closure(C_word **ptr, int cells, C_word proc, ...)
{
  va_list va;
  C_word *p = *ptr,
         *p0 = p;

  *p = C_CLOSURE_TYPE | cells;
  *(++p) = proc;

  for(va_start(va, proc); --cells; *(++p) = va_arg(va, C_word));

  va_end(va);
  *ptr = p + 1;
  return (C_word)p0;
}


/* obsolete: replaced by C_a_pair in chicken.h */
C_regparm C_word C_fcall C_pair(C_word **ptr, C_word car, C_word cdr)
{
  C_word *p = *ptr,
         *p0 = p;
 
  *(p++) = C_PAIR_TYPE | (C_SIZEOF_PAIR - 1);
  *(p++) = car;
  *(p++) = cdr;
  *ptr = p;
  return (C_word)p0;
}


C_regparm C_word C_fcall C_number(C_word **ptr, double n)
{
  C_word 
    *p = *ptr,
    *p0;
  double m;

  if(n <= (double)C_MOST_POSITIVE_FIXNUM
     && n >= (double)C_MOST_NEGATIVE_FIXNUM && modf(n, &m) == 0.0) {
    return C_fix(n);
  }

#ifndef C_SIXTY_FOUR
#ifndef C_DOUBLE_IS_32_BITS
  /* Align double on 8-byte boundary: */
  if(C_aligned8(p)) ++p;
#endif
#endif

  p0 = p;
  *(p++) = C_FLONUM_TAG;
  *((double *)p) = n;
  *ptr = p + sizeof(double) / sizeof(C_word);
  return (C_word)p0;
}


C_regparm C_word C_fcall C_mpointer(C_word **ptr, void *mp)
{
  C_word 
    *p = *ptr,
    *p0 = p;

  *(p++) = C_POINTER_TYPE | 1;
  *((void **)p) = mp;
  *ptr = p + 1;
  return (C_word)p0;
}


C_regparm C_word C_fcall C_mpointer_or_false(C_word **ptr, void *mp)
{
  C_word 
    *p = *ptr,
    *p0 = p;

  if(mp == NULL) return C_SCHEME_FALSE;

  *(p++) = C_POINTER_TYPE | 1;
  *((void **)p) = mp;
  *ptr = p + 1;
  return (C_word)p0;
}


C_regparm C_word C_fcall C_taggedmpointer(C_word **ptr, C_word tag, void *mp)
{
  C_word 
    *p = *ptr,
    *p0 = p;

  *(p++) = C_TAGGED_POINTER_TAG;
  *((void **)p) = mp;
  *(++p) = tag;
  *ptr = p + 1;
  return (C_word)p0;
}


C_regparm C_word C_fcall C_taggedmpointer_or_false(C_word **ptr, C_word tag, void *mp)
{
  C_word 
    *p = *ptr,
    *p0 = p;

  if(mp == NULL) return C_SCHEME_FALSE;
 
  *(p++) = C_TAGGED_POINTER_TAG;
  *((void **)p) = mp;
  *(++p) = tag;
  *ptr = p + 1;
  return (C_word)p0;
}


C_regparm C_word C_fcall C_swigmpointer(C_word **ptr, void *mp, void *sdata)
{
  C_word 
    *p = *ptr,
    *p0 = p;

  *(p++) = C_SWIG_POINTER_TAG;
  *((void **)p) = mp;
  *((void **)p + 1) = sdata;
  *ptr = p + 2;
  return (C_word)p0;
}


C_word C_vector(C_word **ptr, int n, ...)
{
  va_list v;
  C_word 
    *p = *ptr,
    *p0 = p; 

  *(p++) = C_VECTOR_TYPE | n;
  va_start(v, n);

  while(n--)
    *(p++) = va_arg(v, C_word);

  *ptr = p;
  va_end(v);
  return (C_word)p0;
}


C_word C_structure(C_word **ptr, int n, ...)
{
  va_list v;
  C_word *p = *ptr,
         *p0 = p; 

  *(p++) = C_STRUCTURE_TYPE | n;
  va_start(v, n);

  while(n--)
    *(p++) = va_arg(v, C_word);

  *ptr = p;
  va_end(v);
  return (C_word)p0;
}


C_regparm C_word C_fcall
C_mutate_slot(C_word *slot, C_word val)
{
  unsigned int mssize, newmssize, bytes;

  ++mutation_count;
  /* Mutation stack exists to track mutations pointing from elsewhere
   * into nursery.  Stuff pointing anywhere else can be skipped, as
   * well as mutations on nursery objects.
   */
  if(!C_in_stackp(val) || C_in_stackp((C_word)slot))
    return *slot = val;

#ifdef C_GC_HOOKS
  if(C_gc_mutation_hook != NULL && C_gc_mutation_hook(slot, val)) return val;
#endif

  if(mutation_stack_top >= mutation_stack_limit) {
    assert(mutation_stack_top == mutation_stack_limit);
    mssize = mutation_stack_top - mutation_stack_bottom;
    newmssize = mssize * 2;
    bytes = newmssize * sizeof(C_word *);

    if(debug_mode) 
      C_dbg(C_text("debug"), C_text("resizing mutation stack from %uk to %uk ...\n"),
	    (mssize * sizeof(C_word *)) / 1024, bytes / 1024);

    mutation_stack_bottom = (C_word **)realloc(mutation_stack_bottom, bytes);

    if(mutation_stack_bottom == NULL)
      panic(C_text("out of memory - cannot re-allocate mutation stack"));

    mutation_stack_limit = mutation_stack_bottom + newmssize;
    mutation_stack_top = mutation_stack_bottom + mssize;
  }

  *(mutation_stack_top++) = slot;
  ++tracked_mutation_count;
  return *slot = val;
}


/* Initiate garbage collection: */


void C_save_and_reclaim(void *trampoline, int n, C_word *av)
{
  C_word new_size = nmax(1UL << (int)ceil(log2(n)), DEFAULT_TEMPORARY_STACK_SIZE);

  assert(av > C_temporary_stack_bottom || av < C_temporary_stack_limit);
  assert(C_temporary_stack == C_temporary_stack_bottom);

  /* Don't *immediately* slam back to default size */
  if (new_size < temporary_stack_size / 4)
    new_size = temporary_stack_size >> 1;

  if (new_size != temporary_stack_size) {

    if(fixed_temporary_stack_size)
      panic(C_text("fixed temporary stack overflow (\"apply\" called with too many arguments?)"));


    if(gc_report_flag) {
      C_dbg(C_text("GC"), C_text("resizing temporary stack dynamically from " UWORD_COUNT_FORMAT_STRING "k to " UWORD_COUNT_FORMAT_STRING "k ...\n"),
            C_wordstobytes(temporary_stack_size) / 1024,
            C_wordstobytes(new_size) / 1024);
    }

    C_free(C_temporary_stack_limit);

    if((C_temporary_stack_limit = (C_word *)C_malloc(new_size * sizeof(C_word))) == NULL)
      panic(C_text("out of memory - could not resize temporary stack"));

    C_temporary_stack_bottom = C_temporary_stack_limit + new_size;
    C_temporary_stack = C_temporary_stack_bottom;
    temporary_stack_size = new_size;
  }

  C_temporary_stack = C_temporary_stack_bottom - n;

  assert(C_temporary_stack >= C_temporary_stack_limit);

  C_memmove(C_temporary_stack, av, n * sizeof(C_word));
  C_reclaim(trampoline, n);
}


void C_save_and_reclaim_args(void *trampoline, int n, ...)
{
  va_list v;
  int i;
  
  va_start(v, n);

  for(i = 0; i < n; ++i)
    C_save(va_arg(v, C_word));

  va_end(v);
  C_reclaim(trampoline, n);
}


#ifdef __SUNPRO_C
static void mark(C_word *x) { \
  C_word *_x = (x), _val = *_x; \
  if(!C_immediatep(_val)) really_mark(_x); \
}
#else
# define mark(x)				\
  C_cblock						\
  C_word *_x = (x), _val = *_x;				\
  if(!C_immediatep(_val)) really_mark(_x);		\
  C_cblockend
#endif


C_regparm void C_fcall C_reclaim(void *trampoline, C_word c)
{
  int i, j, n, fcount, weakn = 0;
  C_uword count, bytes;
  C_word *p, **msp, bucket, last, item, container;
  C_header h;
  C_byte *tmp, *start;
  LF_LIST *lfn;
  C_SCHEME_BLOCK *bp;
  C_GC_ROOT *gcrp;
  WEAK_TABLE_ENTRY *wep;
  double tgc = 0;
  C_SYMBOL_TABLE *stp;
  volatile int finalizers_checked;
  FINALIZER_NODE *flist;
  TRACE_INFO *tinfo;
  C_DEBUG_INFO cell;

  /* assert(C_timer_interrupt_counter >= 0); */

  if(pending_interrupts_count > 0 && C_interrupts_enabled) {
    stack_check_demand = 0; /* forget demand: we're not going to gc yet */
    handle_interrupt(trampoline);
  }

  cell.enabled = 0;
  cell.event = C_DEBUG_GC;
  cell.loc = "<runtime>";
  cell.val = "GC_MINOR";
  C_debugger(&cell, 0, NULL);

  /* Note: the mode argument will always be GC_MINOR or GC_REALLOC. */
  if(C_pre_gc_hook != NULL) C_pre_gc_hook(GC_MINOR);

  finalizers_checked = 0;
  C_restart_trampoline = (C_proc)trampoline;
  C_restart_c = c;
  heap_scan_top = (C_byte *)C_align((C_uword)C_fromspace_top);
  gc_mode = GC_MINOR;
  start = C_fromspace_top;

  if(C_enable_gcweak) 
    weak_table_randomization = rand();

  /* Entry point for second-level GC (on explicit request or because of full fromspace): */
#ifdef HAVE_SIGSETJMP
  if(C_sigsetjmp(gc_restart, 0) || start >= C_fromspace_limit) {
#else
  if(C_setjmp(gc_restart) || start >= C_fromspace_limit) {
#endif
    if(gc_bell) {
      C_putchar(7);
      C_fflush(stdout);
    }

    tgc = C_cpu_milliseconds();

    if(gc_mode == GC_REALLOC) {
      cell.val = "GC_REALLOC";
      C_debugger(&cell, 0, NULL);
      C_rereclaim2(percentage(heap_size, C_heap_growth), 0);
      gc_mode = GC_MAJOR;
      count = (C_uword)tospace_top - (C_uword)tospace_start;
      goto i_like_spaghetti;
    }

    heap_scan_top = (C_byte *)C_align((C_uword)tospace_top);    
    gc_mode = GC_MAJOR;
    cell.val = "GC_MAJOR";
    C_debugger(&cell, 0, NULL);

    /* Mark items in forwarding table: */
    for(p = forwarding_table; *p != 0; p += 2) {
      last = p[ 1 ];
      mark(&p[ 1 ]);
      C_block_header(p[ 0 ]) = C_block_header(last);
    }

    /* Mark literal frames: */
    for(lfn = lf_list; lfn != NULL; lfn = lfn->next)
      for(i = 0; i < lfn->count; ++i)
        mark(&lfn->lf[i]);

    /* Mark symbol tables: */
    for(stp = symbol_table_list; stp != NULL; stp = stp->next)
      for(i = 0; i < stp->size; ++i)
        mark(&stp->table[i]);

    /* Mark collectibles: */
    for(msp = collectibles; msp < collectibles_top; ++msp)
      if(*msp != NULL) mark(*msp);

    /* mark normal GC roots: */
    for(gcrp = gc_root_list; gcrp != NULL; gcrp = gcrp->next) {
      if(!gcrp->finalizable) mark(&gcrp->value);
    }

    mark_system_globals();
  }
  else {
    /* Mark mutated slots: */
    for(msp = mutation_stack_bottom; msp < mutation_stack_top; ++msp)
      mark(*msp);
  }

  assert(C_temporary_stack >= C_temporary_stack_limit);

  /* Clear the mutated slot stack: */
  mutation_stack_top = mutation_stack_bottom;

  /* Mark live values: */
  for(p = C_temporary_stack; p < C_temporary_stack_bottom; ++p)
    mark(p);

  /* Mark trace-buffer: */
  for(tinfo = trace_buffer; tinfo < trace_buffer_limit; ++tinfo) {
    mark(&tinfo->cooked1);
    mark(&tinfo->cooked2);
    mark(&tinfo->thread);
  }

 rescan:
  /* Mark nested values in already moved (marked) blocks in breadth-first manner: */
  while(heap_scan_top < (gc_mode == GC_MINOR ? C_fromspace_top : tospace_top)) {
    bp = (C_SCHEME_BLOCK *)heap_scan_top;

    if(*((C_word *)bp) == ALIGNMENT_HOLE_MARKER) 
      bp = (C_SCHEME_BLOCK *)((C_word *)bp + 1);

    n = C_header_size(bp);
    h = bp->header;
    bytes = (h & C_BYTEBLOCK_BIT) ? n : n * sizeof(C_word);
    p = bp->data;

    if(n > 0 && (h & C_BYTEBLOCK_BIT) == 0) {
      if(h & C_SPECIALBLOCK_BIT) {
	--n;
	++p;
      }

      while(n--) mark(p++);
    }

    heap_scan_top = (C_byte *)bp + C_align(bytes) + sizeof(C_word);
  }

  if(gc_mode == GC_MINOR) {
    count = (C_uword)C_fromspace_top - (C_uword)start;
    ++gc_count_1;
    ++gc_count_1_total;
    update_locative_table(GC_MINOR);
  }
  else {
    if(!finalizers_checked) {
      /* Mark finalizer list and remember pointers to non-forwarded items: */
      last = C_block_item(pending_finalizers_symbol, 0);

      if(!C_immediatep(last) && (j = C_unfix(C_block_item(last, 0))) != 0) { 
	/* still finalizers pending: just mark table items... */
	if(gc_report_flag)
	  C_dbg(C_text("GC"), C_text("%d finalized item(s) still pending\n"), j);

	j = fcount = 0;

	for(flist = finalizer_list; flist != NULL; flist = flist->next) {
	  mark(&flist->item);
	  mark(&flist->finalizer);
	  ++fcount;
	}

	/* mark finalizable GC roots: */
	for(gcrp = gc_root_list; gcrp != NULL; gcrp = gcrp->next) {
	  if(gcrp->finalizable) mark(&gcrp->value);
	}

	if(gc_report_flag && fcount > 0)
	  C_dbg(C_text("GC"), C_text("%d finalizer value(s) marked\n"), fcount);
      }
      else {
	j = fcount = 0;

	/* move into pending */
	for(flist = finalizer_list; flist != NULL; flist = flist->next) {
	  if(j < C_max_pending_finalizers) {
	    if(!is_fptr(C_block_header(flist->item))) 
	      pending_finalizer_indices[ j++ ] = flist;
	  }
	}

	/* mark */
	for(flist = finalizer_list; flist != NULL; flist = flist->next) {
	  mark(&flist->item);
	  mark(&flist->finalizer);
	}

	/* mark finalizable GC roots: */
	for(gcrp = gc_root_list; gcrp != NULL; gcrp = gcrp->next) {
	  if(gcrp->finalizable) mark(&gcrp->value);
	}
      }

      pending_finalizer_count = j;
      finalizers_checked = 1;

      if(pending_finalizer_count > 0 && gc_report_flag)
	C_dbg(C_text("GC"), C_text("%d finalizer(s) pending (%d live)\n"), 
	      pending_finalizer_count, live_finalizer_count);

      goto rescan;
    }
    else {
      /* Copy finalized items with remembered indices into `##sys#pending-finalizers' 
	 (and release finalizer node): */
      if(pending_finalizer_count > 0) {
	if(gc_report_flag)
	  C_dbg(C_text("GC"), C_text("queueing %d finalizer(s)\n"), pending_finalizer_count);

	last = C_block_item(pending_finalizers_symbol, 0);
	assert(C_block_item(last, 0) == C_fix(0));
	C_set_block_item(last, 0, C_fix(pending_finalizer_count));

	for(i = 0; i < pending_finalizer_count; ++i) {
	  flist = pending_finalizer_indices[ i ];
	  C_set_block_item(last, 1 + i * 2, flist->item);
	  C_set_block_item(last, 2 + i * 2, flist->finalizer);
	  
	  if(flist->previous != NULL) flist->previous->next = flist->next;
	  else finalizer_list = flist->next;

	  if(flist->next != NULL) flist->next->previous = flist->previous;

	  flist->next = finalizer_free_list;
	  flist->previous = NULL;
	  finalizer_free_list = flist;
	  --live_finalizer_count;
	}
      }
    }

    update_locative_table(gc_mode);
    count = (C_uword)tospace_top - (C_uword)tospace_start;

    /*** isn't gc_mode always GC_MAJOR here? */
    if(gc_mode == GC_MAJOR && 
       count < percentage(percentage(heap_size, C_heap_shrinkage), DEFAULT_HEAP_SHRINKAGE_USED) &&
       heap_size > MINIMAL_HEAP_SIZE && !C_heap_size_is_fixed)
      C_rereclaim2(percentage(heap_size, C_heap_shrinkage), 0);
    else {
      C_fromspace_top = tospace_top;
      tmp = fromspace_start;
      fromspace_start = tospace_start;
      tospace_start = tospace_top = tmp;
      tmp = C_fromspace_limit;
      C_fromspace_limit = tospace_limit;
      tospace_limit = tmp;
    }

  i_like_spaghetti:
    ++gc_count_2;

    if(C_enable_gcweak) {
      /* Check entries in weak item table and recover items ref'd only
         once, which are unbound symbols and have empty property-lists: */
      weakn = 0;
      wep = weak_item_table;

      for(i = 0; i < WEAK_TABLE_SIZE; ++i, ++wep)
	if(wep->item != 0) { 
	  if((wep->container & WEAK_COUNTER_MAX) == 0 && /* counter saturated? (more than 1) */
	     is_fptr((item = C_block_header(wep->item)))) { /* and forwarded/collected */
	    item = fptr_to_ptr(item);			    /* recover obj from forwarding ptr */
	    container = wep->container & ~WEAK_COUNTER_MASK;

	    if(C_header_bits(item) == C_SYMBOL_TYPE && 
	       C_block_item(item, 0) == C_SCHEME_UNBOUND &&
	       C_block_item(item, 2) == C_SCHEME_END_OF_LIST) {
	      ++weakn;
	      C_set_block_item(container, 0, C_SCHEME_UNDEFINED); /* clear reference to item */
	    }
	  }

	  wep->item = wep->container = 0;
	}

      /* Remove empty buckets in symbol table: */
      for(stp = symbol_table_list; stp != NULL; stp = stp->next) {
	for(i = 0; i < stp->size; ++i) {
	  last = 0;
	  
	  for(bucket = stp->table[ i ]; bucket != C_SCHEME_END_OF_LIST; bucket = C_block_item(bucket,1))
	    if(C_block_item(bucket,0) == C_SCHEME_UNDEFINED) {
	      if(last) C_set_block_item(last, 1, C_block_item(bucket,1));
	      else stp->table[ i ] = C_block_item(bucket,1);
	    }
	    else last = bucket;
	}
      }
    }
  }

  if(gc_mode == GC_MAJOR) {
    tgc = C_cpu_milliseconds() - tgc;
    gc_ms += tgc;
    timer_accumulated_gc_ms += tgc;
  }

  /* Display GC report: 
     Note: stubbornly writes to stderr - there is no provision for other output-ports */
  if(gc_report_flag == 1 || (gc_report_flag && gc_mode == GC_MAJOR)) {
    C_dbg(C_text("GC"), C_text("level  %d\tgcs(minor)  %d\tgcs(major)  %d\n"),
	  gc_mode, gc_count_1, gc_count_2);
    i = (C_uword)C_stack_pointer;

#if C_STACK_GROWS_DOWNWARD
    C_dbg("GC", C_text("stack\t" UWORD_FORMAT_STRING "\t" UWORD_FORMAT_STRING "\t" UWORD_FORMAT_STRING), 
	  (C_uword)C_stack_limit, (C_uword)i, (C_uword)C_stack_limit + stack_size);
#else
    C_dbg("GC", C_text("stack\t" UWORD_FORMAT_STRING "\t" UWORD_FORMAT_STRING "\t" UWORD_FORMAT_STRING), 
	  (C_uword)C_stack_limit - stack_size, (C_uword)i, (C_uword)C_stack_limit);
#endif

    if(gc_mode == GC_MINOR) 
      C_fprintf(C_stderr, C_text("\t" UWORD_FORMAT_STRING), (C_uword)count);

    C_fputc('\n', C_stderr);
    C_dbg("GC", C_text(" from\t" UWORD_FORMAT_STRING "\t" UWORD_FORMAT_STRING "\t" UWORD_FORMAT_STRING),
	  (C_uword)fromspace_start, (C_uword)C_fromspace_top, (C_uword)C_fromspace_limit);

    if(gc_mode == GC_MAJOR) 
      C_fprintf(C_stderr, C_text("\t" UWORD_FORMAT_STRING), (C_uword)count);

    C_fputc('\n', C_stderr);
    C_dbg("GC", C_text("   to\t" UWORD_FORMAT_STRING "\t" UWORD_FORMAT_STRING "\t" UWORD_FORMAT_STRING" \n"), 
	  (C_uword)tospace_start, (C_uword)tospace_top, 
	  (C_uword)tospace_limit);

    if(gc_mode == GC_MAJOR && C_enable_gcweak && weakn)
      C_dbg("GC", C_text("%d recoverable weakly held items found\n"), weakn);
    
    C_dbg("GC", C_text("%d locatives (from %d)\n"), locative_table_count, locative_table_size);
  }

  if(gc_mode == GC_MAJOR) {
    gc_count_1 = 0;
    maximum_heap_usage = count > maximum_heap_usage ? count : maximum_heap_usage;
  }

  if(C_post_gc_hook != NULL) C_post_gc_hook(gc_mode, (C_long)tgc);

  /* Unwind stack completely */
#ifdef HAVE_SIGSETJMP
  C_siglongjmp(C_restart, 1);
#else
  C_longjmp(C_restart, 1);
#endif
}


C_regparm void C_fcall mark_system_globals(void)
{
  mark(&interrupt_hook_symbol);
  mark(&error_hook_symbol);
  mark(&callback_continuation_stack_symbol);
  mark(&pending_finalizers_symbol);
  mark(&current_thread_symbol);
}


C_regparm void C_fcall really_mark(C_word *x)
{
  C_word val, item;
  C_uword n, bytes;
  C_header h;
  C_SCHEME_BLOCK *p, *p2;
  WEAK_TABLE_ENTRY *wep;

  val = *x;

  if (!C_in_stackp(val) && !C_in_heapp(val)) {
#ifdef C_GC_HOOKS
      if(C_gc_trace_hook != NULL) 
	C_gc_trace_hook(x, gc_mode);
#endif

      return;
  }

  p = (C_SCHEME_BLOCK *)val;
  
  h = p->header;

  if(gc_mode == GC_MINOR) {
    if(is_fptr(h)) {
      *x = val = fptr_to_ptr(h);
      return;
    }

    if((C_uword)val >= (C_uword)fromspace_start && (C_uword)val < (C_uword)C_fromspace_top) return;

    p2 = (C_SCHEME_BLOCK *)C_align((C_uword)C_fromspace_top);

#ifndef C_SIXTY_FOUR
    if((h & C_8ALIGN_BIT) && C_aligned8(p2) && (C_byte *)p2 < C_fromspace_limit) {
      *((C_word *)p2) = ALIGNMENT_HOLE_MARKER;
      p2 = (C_SCHEME_BLOCK *)((C_word *)p2 + 1);
    }
#endif

    n = C_header_size(p);
    bytes = (h & C_BYTEBLOCK_BIT) ? n : n * sizeof(C_word);

    if(((C_byte *)p2 + bytes + sizeof(C_word)) > C_fromspace_limit)
#ifdef HAVE_SIGSETJMP
      C_siglongjmp(gc_restart, 1);
#else
      C_longjmp(gc_restart, 1);
#endif

    C_fromspace_top = (C_byte *)p2 + C_align(bytes) + sizeof(C_word);

  scavenge:
    *x = (C_word)p2;
    p2->header = h;
    p->header = ptr_to_fptr((C_uword)p2);
    C_memcpy(p2->data, p->data, bytes);
  }
  else { /* (major GC) */
    /* Increase counter (saturated at 2) if weakly held item (someone pointed to this object): */
    if(C_enable_gcweak &&
       (h & C_HEADER_TYPE_BITS) == C_SYMBOL_TYPE &&
       (wep = lookup_weak_table_entry(val, 0)) != NULL) {
      if((wep->container & WEAK_COUNTER_MAX) == 0) ++wep->container;
    }

    if(is_fptr(h)) {
      val = fptr_to_ptr(h);

      /* When we marked the bucket, it may have already referred to
       * the moved symbol instead of its original location. Re-check:
       */
      if(C_enable_gcweak &&
         (C_block_header(val) & C_HEADER_TYPE_BITS) == C_SYMBOL_TYPE &&
         (wep = lookup_weak_table_entry(*x, 0)) != NULL) {
        if((wep->container & WEAK_COUNTER_MAX) == 0) ++wep->container;
      }

      if((C_uword)val >= (C_uword)tospace_start && (C_uword)val < (C_uword)tospace_top) {
	*x = val;
	return;
      }

      /* Link points into fromspace: fetch new pointer + header and copy... */
      p = (C_SCHEME_BLOCK *)val;
      h = p->header;

      if(is_fptr(h)) {
	/* Link points into fromspace and into a link which points into from- or tospace: */
	val = fptr_to_ptr(h);
	
        /* See above: might happen twice */
        if(C_enable_gcweak &&
           (C_block_header(val) & C_HEADER_TYPE_BITS) == C_SYMBOL_TYPE &&
           /* Check both the original and intermediate location: */
           ((wep = lookup_weak_table_entry((C_word)p, 0)) != NULL ||
            (wep = lookup_weak_table_entry(*x, 0)) != NULL)) {
          if((wep->container & WEAK_COUNTER_MAX) == 0) ++wep->container;
        }

	if((C_uword)val >= (C_uword)tospace_start && (C_uword)val < (C_uword)tospace_top) {
	  *x = val;
	  return;
	}

	p = (C_SCHEME_BLOCK *)val;
	h = p->header;
      }
    }

    p2 = (C_SCHEME_BLOCK *)C_align((C_uword)tospace_top);

#ifndef C_SIXTY_FOUR
    if((h & C_8ALIGN_BIT) && C_aligned8(p2) && (C_byte *)p2 < tospace_limit) {
      *((C_word *)p2) = ALIGNMENT_HOLE_MARKER;
      p2 = (C_SCHEME_BLOCK *)((C_word *)p2 + 1);
    }
#endif

    if(C_enable_gcweak && (h & C_HEADER_TYPE_BITS) == C_BUCKET_TYPE) {
      item = C_block_item(val,0);

      /* Lookup item in weak item table or add entry: */
      if((wep = lookup_weak_table_entry(item, (C_word)p2)) != NULL) {
	/* If item is already forwarded, then set count to 2: */
	if(is_fptr(C_block_header(item))) wep->container |= 2;
      }
    }

    n = C_header_size(p);
    bytes = (h & C_BYTEBLOCK_BIT) ? n : n * sizeof(C_word);

    if(((C_byte *)p2 + bytes + sizeof(C_word)) > tospace_limit) {
      /* Detect impossibilities before GC_REALLOC to preserve state: */
      if (C_in_stackp((C_word)p) && bytes > stack_size)
        panic(C_text("Detected corrupted data in stack"));
      if (C_in_heapp((C_word)p) && bytes > (heap_size / 2))
        panic(C_text("Detected corrupted data in heap"));
      if(C_heap_size_is_fixed)
	panic(C_text("out of memory - heap full"));
      
      gc_mode = GC_REALLOC;
#ifdef HAVE_SIGSETJMP
      C_siglongjmp(gc_restart, 1);
#else
      C_longjmp(gc_restart, 1);
#endif
    }

    tospace_top = (C_byte *)p2 + C_align(bytes) + sizeof(C_word);
    goto scavenge;
  }
}


#ifdef __SUNPRO_C
static void remark(C_word *x) { \
  C_word *_x = (x), _val = *_x;		     \
  if(!C_immediatep(_val)) really_remark(_x); \
}
#else
#define remark(x)				\
  C_cblock					\
  C_word *_x = (x), _val = *_x;			\
  if(!C_immediatep(_val)) really_remark(_x);	\
  C_cblockend
#endif


/* Do a major GC into a freshly allocated heap: */

C_regparm void C_fcall C_rereclaim2(C_uword size, int relative_resize)
{
  int i, j;
  C_uword count, n, bytes;
  C_word *p, **msp, item, last;
  C_header h;
  C_byte *tmp, *start;
  LF_LIST *lfn;
  C_SCHEME_BLOCK *bp;
  WEAK_TABLE_ENTRY *wep;
  C_GC_ROOT *gcrp;
  C_SYMBOL_TABLE *stp;
  FINALIZER_NODE *flist;
  TRACE_INFO *tinfo;
  C_byte *new_heapspace;
  size_t  new_heapspace_size;

  if(C_pre_gc_hook != NULL) C_pre_gc_hook(GC_REALLOC);

  /*
   * Normally, size is "absolute": it indicates the desired size of
   * the entire new heap.  With relative_resize, size is a demanded
   * increase of the heap, so we'll have to add it.  This calculation
   * doubles the current heap size because heap_size is already both
   * halves.  We add size*2 because we'll eventually divide the size
   * by 2 for both halves.  We also add stack_size*2 because all the
   * nursery data is also copied to the heap on GC, and the requested
   * memory "size" must be available after the GC.
   */
  if(relative_resize) size = (heap_size + size + stack_size) * 2;

  if(size < MINIMAL_HEAP_SIZE) size = MINIMAL_HEAP_SIZE;

  /*
   * When heap grows, ensure it's enough to accommodate first
   * generation (nursery).  Because we're calculating the total heap
   * size here (fromspace *AND* tospace), we have to double the stack
   * size, otherwise we'd accommodate only half the stack in the tospace.
   */
  if(size > heap_size && size - heap_size < stack_size * 2)
    size = heap_size + stack_size * 2;

  /*
   * The heap has grown but we've already hit the maximal size with the current
   * heap, we can't do anything else but panic.
   */
  if(size > heap_size && heap_size >= C_maximal_heap_size)
    panic(C_text("out of memory - heap has reached its maximum size"));

  if(size > C_maximal_heap_size) size = C_maximal_heap_size;

  if(debug_mode) {
    C_dbg(C_text("debug"), C_text("resizing heap dynamically from "
                                  UWORD_COUNT_FORMAT_STRING "k to "
                                  UWORD_COUNT_FORMAT_STRING "k ...\n"),
	  heap_size / 1024, size / 1024);
  }

  if(gc_report_flag) {
    C_dbg(C_text("GC"), C_text("(old) fromspace: \tstart=" UWORD_FORMAT_STRING 
			       ", \tlimit=" UWORD_FORMAT_STRING "\n"),
	  (C_word)fromspace_start, (C_word)C_fromspace_limit);
    C_dbg(C_text("GC"), C_text("(old) tospace:   \tstart=" UWORD_FORMAT_STRING
			       ", \tlimit=" UWORD_FORMAT_STRING "\n"),
	  (C_word)tospace_start, (C_word)tospace_limit);
  }

  heap_size = size;         /* Total heap size of the two halves... */
  size /= 2;                /* ...each half is this big */
  
  /*
   * Start by allocating the new heap's fromspace.  After remarking,
   * allocate the other half of the new heap (its tospace).
   *
   * To clarify: what we call "new_space" here is what will eventually
   * be cycled over to "fromspace" when re-reclamation has finished
   * (that is, after the old one has been freed).
   */
  if ((new_heapspace = heap_alloc (size, &new_tospace_start)) == NULL)
    panic(C_text("out of memory - cannot allocate heap segment"));
  new_heapspace_size = size;

  new_tospace_top = new_tospace_start;
  new_tospace_limit = new_tospace_start + size;
  heap_scan_top = new_tospace_top;

  /* Mark items in forwarding table: */
  for(p = forwarding_table; *p != 0; p += 2) {
    last = p[ 1 ];
    remark(&p[ 1 ]);
    C_block_header(p[ 0 ]) = C_block_header(last);
  }

  /* Mark literal frames: */
  for(lfn = lf_list; lfn != NULL; lfn = lfn->next)
    for(i = 0; i < lfn->count; ++i)
      remark(&lfn->lf[i]);

  /* Mark symbol table: */
  for(stp = symbol_table_list; stp != NULL; stp = stp->next)
    for(i = 0; i < stp->size; ++i)
      remark(&stp->table[i]);

  /* Mark collectibles: */
  for(msp = collectibles; msp < collectibles_top; ++msp)
    if(*msp != NULL) remark(*msp);

  for(gcrp = gc_root_list; gcrp != NULL; gcrp = gcrp->next)
    remark(&gcrp->value);

  remark_system_globals();

  /* Clear the mutated slot stack: */
  mutation_stack_top = mutation_stack_bottom;

  /* Mark live values: */
  for(p = C_temporary_stack; p < C_temporary_stack_bottom; ++p)
    remark(p);

  /* Mark locative table: */
  for(i = 0; i < locative_table_count; ++i)
    remark(&locative_table[ i ]);

  /* Mark finalizer table: */
  for(flist = finalizer_list; flist != NULL; flist = flist->next) {
    remark(&flist->item);
    remark(&flist->finalizer);
  }

  /* Clear weakly held items: */
  if(C_enable_gcweak) {
    wep = weak_item_table; 

    for(i = 0; i < WEAK_TABLE_SIZE; ++i, ++wep)
      wep->item = wep->container = 0;
  }

  /* Mark trace-buffer: */
  for(tinfo = trace_buffer; tinfo < trace_buffer_limit; ++tinfo) {
    remark(&tinfo->cooked1);
    remark(&tinfo->cooked2);
    remark(&tinfo->thread);
  }

  update_locative_table(GC_REALLOC);

  /* Mark nested values in already moved (marked) blocks in breadth-first manner: */
  while(heap_scan_top < new_tospace_top) {
    bp = (C_SCHEME_BLOCK *)heap_scan_top;

    if(*((C_word *)bp) == ALIGNMENT_HOLE_MARKER) 
      bp = (C_SCHEME_BLOCK *)((C_word *)bp + 1);

    n = C_header_size(bp);
    h = bp->header;
    assert(!is_fptr(h));
    bytes = (h & C_BYTEBLOCK_BIT) ? n : n * sizeof(C_word);
    p = bp->data;

    if(n > 0 && (h & C_BYTEBLOCK_BIT) == 0) {
      if(h & C_SPECIALBLOCK_BIT) {
	--n;
	++p;
      }

      while(n--) remark(p++);
    }

    heap_scan_top = (C_byte *)bp + C_align(bytes) + sizeof(C_word);
  }

  heap_free (heapspace1, heapspace1_size);
  heap_free (heapspace2, heapspace2_size);
  
  if ((heapspace2 = heap_alloc (size, &tospace_start)) == NULL)
    panic(C_text("out of memory - cannot allocate next heap segment"));
  heapspace2_size = size;

  heapspace1 = new_heapspace;
  heapspace1_size = new_heapspace_size;
  tospace_limit = tospace_start + size;
  tospace_top = tospace_start;
  fromspace_start = new_tospace_start;
  C_fromspace_top = new_tospace_top;
  C_fromspace_limit = new_tospace_limit;

  if(gc_report_flag) {
    C_dbg(C_text("GC"), C_text("resized heap to %d bytes\n"), heap_size);
    C_dbg(C_text("GC"), C_text("(new) fromspace: \tstart=" UWORD_FORMAT_STRING 
			       ", \tlimit=" UWORD_FORMAT_STRING "\n"),
	  (C_word)fromspace_start, (C_word)C_fromspace_limit);
    C_dbg(C_text("GC"), C_text("(new) tospace:   \tstart=" UWORD_FORMAT_STRING
			       ", \tlimit=" UWORD_FORMAT_STRING "\n"),
	  (C_word)tospace_start, (C_word)tospace_limit);
  }

  if(C_post_gc_hook != NULL) C_post_gc_hook(GC_REALLOC, 0);
}


C_regparm void C_fcall remark_system_globals(void)
{
  remark(&interrupt_hook_symbol);
  remark(&error_hook_symbol);
  remark(&callback_continuation_stack_symbol);
  remark(&pending_finalizers_symbol);
  remark(&current_thread_symbol);
}


C_regparm void C_fcall really_remark(C_word *x)
{
  C_word val, item;
  C_uword n, bytes;
  C_header h;
  C_SCHEME_BLOCK *p, *p2;
  WEAK_TABLE_ENTRY *wep;

  val = *x;

  if (!C_in_stackp(val) && !C_in_heapp(val) && !C_in_new_heapp(val)) {
#ifdef C_GC_HOOKS
      if(C_gc_trace_hook != NULL) 
	C_gc_trace_hook(x, gc_mode);
#endif

      return;
  }

  p = (C_SCHEME_BLOCK *)val;
  
  h = p->header;

  if(is_fptr(h)) {
    val = fptr_to_ptr(h);

    if((C_uword)val >= (C_uword)new_tospace_start && (C_uword)val < (C_uword)new_tospace_top) {
      *x = val;
      return;
    }

    /* Link points into nursery, fromspace or the old tospace:
    * fetch new pointer + header and copy... */
    p = (C_SCHEME_BLOCK *)val;
    h = p->header;
    n = 1;

    while(is_fptr(h)) {
      /* Link points into fromspace or old tospace and into a link which
       * points into tospace or new-tospace: */
      val = fptr_to_ptr(h);
	
      if((C_uword)val >= (C_uword)new_tospace_start && (C_uword)val < (C_uword)new_tospace_top) {
	*x = val;
	return;
      }

      p = (C_SCHEME_BLOCK *)val;
      h = p->header;

      if(++n > 3)
	panic(C_text("forwarding chain during re-reclamation is longer than 3. somethings fishy."));
    }
  }

  p2 = (C_SCHEME_BLOCK *)C_align((C_uword)new_tospace_top);

#ifndef C_SIXTY_FOUR
  if((h & C_8ALIGN_BIT) && C_aligned8(p2) && (C_byte *)p2 < new_tospace_limit) {
    *((C_word *)p2) = ALIGNMENT_HOLE_MARKER;
    p2 = (C_SCHEME_BLOCK *)((C_word *)p2 + 1);
  }
#endif

  n = C_header_size(p);
  bytes = (h & C_BYTEBLOCK_BIT) ? n : n * sizeof(C_word);

  new_tospace_top = ((C_byte *)p2 + C_align(bytes) + sizeof(C_word));
  if(new_tospace_top > new_tospace_limit) {
    panic(C_text("out of memory - heap full while resizing"));
  }

  *x = (C_word)p2;
  p2->header = h;
  assert(!is_fptr(h));
  p->header = ptr_to_fptr((C_word)p2);
  C_memcpy(p2->data, p->data, bytes);
}


C_regparm void C_fcall update_locative_table(int mode)
{
  int i, hi = 0, invalidated = 0;
  C_header h;
  C_word loc, obj, obj2, offset, loc2, ptr;
  C_uword ptr2;

  for(i = 0; i < locative_table_count; ++i) {
    loc = locative_table[ i ];

    if(loc != C_SCHEME_UNDEFINED) {
      h = C_block_header(loc);

      switch(mode) {
      case GC_MINOR:
        if(is_fptr(h))		/* forwarded? update l-table entry */
          loc = locative_table[ i ] = fptr_to_ptr(h);
        /* otherwise it must have been GC'd (since this is a minor one) */
        else if(C_in_stackp(loc)) {
          locative_table[ i ] = C_SCHEME_UNDEFINED;
          C_set_block_item(loc, 0, 0);
	  ++invalidated;
          break;
        }

        /* forwarded. fix up ptr and check pointed-at object for being forwarded... */
        ptr = C_block_item(loc, 0);
        offset = C_unfix(C_block_item(loc, 1));
        obj = ptr - offset;
        h = C_block_header(obj);

        if(is_fptr(h)) {	/* pointed-at object forwarded? update */
          C_set_block_item(loc, 0, (C_uword)fptr_to_ptr(h) + offset);
	  hi = i + 1;
	}
        else if(C_in_stackp(obj)) { /* pointed-at object GC'd, locative is invalid */
          locative_table[ i ] = C_SCHEME_UNDEFINED;
          C_set_block_item(loc, 0, 0);
        }
	else hi = i + 1;
        
        break;

      case GC_MAJOR:
        if(is_fptr(h))		/* forwarded? update l-table entry */
          loc = locative_table[ i ] = fptr_to_ptr(h);
        else {			/* otherwise, throw away */
          locative_table[ i ] = C_SCHEME_UNDEFINED;
          C_set_block_item(loc, 0, 0);
	  ++invalidated;
          break;
        }

        h = C_block_header(loc);
        
        if(is_fptr(h))		/* new instance is forwarded itself? update again */
          loc = locative_table[ i ] = fptr_to_ptr(h);

        ptr = C_block_item(loc, 0); /* fix up ptr */
        offset = C_unfix(C_block_item(loc, 1));
        obj = ptr - offset;
        h = C_block_header(obj);

        if(is_fptr(h)) {	/* pointed-at object has been forwarded? */
	  ptr2 = (C_uword)fptr_to_ptr(h);
	  h = C_block_header(ptr2);

	  if(is_fptr(h)) {	/* secondary forwarding check for pointed-at object */
	    ptr2 = (C_uword)fptr_to_ptr(h) + offset;
	    C_set_block_item(loc, 0, ptr2);
	  }
	  else C_set_block_item(loc, 0, ptr2 + offset); /* everything's fine, fixup pointer */

	  hi = i + 1;
        }
        else {
          locative_table[ i ] = C_SCHEME_UNDEFINED; /* pointed-at object is dead */
          C_set_block_item(loc, 0, 0);
	  ++invalidated;
        }
        
        break;

      case GC_REALLOC:
        ptr = C_block_item(loc, 0); /* just update ptr's pointed-at objects */
        offset = C_unfix(C_block_item(loc, 1));
        obj = ptr - offset;
        remark(&obj);
        C_set_block_item(loc, 0, obj + offset);        
        break;
      }
    }
  }

  if(gc_report_flag && invalidated > 0)
    C_dbg(C_text("GC"), C_text("locative-table entries reclaimed: %d\n"), invalidated);

  if(mode != GC_REALLOC) locative_table_count = hi;
}


C_regparm WEAK_TABLE_ENTRY *C_fcall lookup_weak_table_entry(C_word item, C_word container)
{
  C_uword
    key = (C_uword)item >> 2,
    disp = 0,
    n;
  WEAK_TABLE_ENTRY *wep;

  for(n = 0; n < WEAK_HASH_ITERATIONS; ++n) {
    key = (key + disp + weak_table_randomization) % WEAK_TABLE_SIZE;
    wep = &weak_item_table[ key ];

    if(wep->item == 0) {
      if(container != 0) {
	/* Add fresh entry: */
	wep->item = item;
	wep->container = container;
	return wep;
      }

      return NULL;
    }
    else if(wep->item == item) return wep;
    else disp += WEAK_HASH_DISPLACEMENT;
  }

  return NULL;
}


void handle_interrupt(void *trampoline)
{
  C_word *p, h, reason, state, proc, n;
  double c;
  C_word av[ 4 ]; 

  /* Build vector with context information: */
  n = C_temporary_stack_bottom - C_temporary_stack;
  /* 18 <=> 2 headers + trampoline + 1 extra slot + 9 for interning + 5 for string */
  p = C_alloc(18 + n);
  proc = (C_word)p;
  *(p++) = C_VECTOR_TYPE | C_BYTEBLOCK_BIT | sizeof(C_word);
  *(p++) = (C_word)trampoline;
  state = (C_word)p;
  *(p++) = C_VECTOR_TYPE | (n + 1);
  *(p++) = proc;
  C_memcpy(p, C_temporary_stack, n * sizeof(C_word));

  /* Restore state to the one at the time of the interrupt: */
  C_temporary_stack = C_temporary_stack_bottom;
  C_stack_limit = C_stack_hard_limit;
  
  /* Invoke high-level interrupt handler: */
  reason = C_fix(pending_interrupts[ --pending_interrupts_count ]);
  proc = C_block_item(interrupt_hook_symbol, 0);

  if(C_immediatep(proc))
    panic(C_text("`##sys#interrupt-hook' is not defined"));

  c = C_cpu_milliseconds() - interrupt_time;
  last_interrupt_latency = c;
  C_timer_interrupt_counter = C_initial_timer_interrupt_period;
  /* <- no continuation is passed: "##sys#interrupt-hook" may not return! */
  av[ 0 ] = proc;
  av[ 1 ] = C_SCHEME_UNDEFINED;
  av[ 2 ] = reason;
  av[ 3 ] = state;
  C_do_apply(4, av);
}


void 
C_unbound_variable(C_word sym)
{
  barf(C_UNBOUND_VARIABLE_ERROR, NULL, sym);
}


/* XXX: This needs to be given a better name.
   C_retrieve used to exist but it just called C_fast_retrieve */
C_regparm C_word C_fcall C_retrieve2(C_word val, char *name)
{
  C_word *p;
  int len;

  if(val == C_SCHEME_UNBOUND) {
    len = C_strlen(name);
    /* this is ok: we won't return from `C_retrieve2'
     * (or the value isn't needed). */
    p = C_alloc(C_SIZEOF_STRING(len));
    C_unbound_variable(C_string2(&p, name));
  }

  return val;
}


void C_ccall C_invalid_procedure(C_word c, C_word *av)
{
  C_word self = av[0];
  barf(C_NOT_A_CLOSURE_ERROR, NULL, self);  
}


C_regparm void *C_fcall C_retrieve2_symbol_proc(C_word val, char *name)
{
  C_word *p;
  int len;

  if(val == C_SCHEME_UNBOUND) {
    len = C_strlen(name);
    /* this is ok: we won't return from `C_retrieve2' (or the value isn't needed). */
    p = C_alloc(C_SIZEOF_STRING(len));
    barf(C_UNBOUND_VARIABLE_ERROR, NULL, C_string2(&p, name));
  }

  return C_fast_retrieve_proc(val);
}

#ifdef C_NONUNIX
VOID CALLBACK win_timer(PVOID data_ignored, BOOLEAN wait_or_fired)
{
  if (profiling) take_profile_sample();
}
#endif

static void set_profile_timer(C_uword freq)
{
#ifdef C_NONUNIX
  static HANDLE timer = NULL;

  if (freq == 0) {
    assert(timer != NULL);
    if (!DeleteTimerQueueTimer(NULL, timer, NULL)) goto error;
    timer = NULL;
  } else if (freq < 1000) {
    panic(C_text("On Windows, sampling can only be done in milliseconds"));
  } else {
    if (!CreateTimerQueueTimer(&timer, NULL, win_timer, NULL, 0, freq/1000, 0))
      goto error;
  }
#else
  struct itimerval itv;

  itv.it_value.tv_sec = freq / 1000000;
  itv.it_value.tv_usec = freq % 1000000;
  itv.it_interval.tv_sec = itv.it_value.tv_sec;
  itv.it_interval.tv_usec = itv.it_value.tv_usec;

  if (setitimer(C_PROFILE_TIMER, &itv, NULL) == -1) goto error;
#endif

  return;

error:
  if (freq == 0) panic(C_text("error clearing timer for profiling"));
  else panic(C_text("error setting timer for profiling"));
}

/* Bump profile count for current top of trace buffer */
static void take_profile_sample()
{
  PROFILE_BUCKET **bp, *b;
  C_char *key;
  TRACE_INFO *tb;
  /* To count distinct calls of a procedure, remember last call */
  static C_char *prev_key = NULL;
  static TRACE_INFO *prev_tb = NULL;

  /* trace_buffer_top points *beyond* the topmost entry: Go back one */
  if (trace_buffer_top == trace_buffer) {
    if (!trace_buffer_full) return; /* No data yet */
    tb = trace_buffer_limit - 1;
  } else {
    tb = trace_buffer_top - 1;
  }

  /* We could also just hash the pointer but that's a bit trickier */
  key = tb->raw;
  bp = profile_table + hash_string(C_strlen(key), key, PROFILE_TABLE_SIZE, 0, 0);
  b = *bp;

  /* First try to find pre-existing item in hash table */
  while(b != NULL) {
    if(b->key == key) {
      b->sample_count++;
      if (prev_key != key && prev_tb != tb)
        b->call_count++;
      goto done;
    }
    else b = b->next;
  }

  /* Not found, allocate a new item and use it as bucket's new head */
  b = next_profile_bucket;
  next_profile_bucket = NULL;

  assert(b != NULL);

  b->next = *bp;
  b->key = key;
  *bp = b;
  b->sample_count = 1;
  b->call_count = 1;

done:
  prev_tb = tb;
  prev_key = key;
}


C_regparm void C_fcall C_trace(C_char *name)
{
  /*
   * When profiling, pre-allocate profile bucket if necessary.  This
   * is used in the signal handler, because it may not malloc.
   */
  if(profiling && next_profile_bucket == NULL) {
    next_profile_bucket = (PROFILE_BUCKET *)C_malloc(sizeof(PROFILE_BUCKET));
    if (next_profile_bucket == NULL) {
      panic(C_text("out of memory - cannot allocate profile table-bucket"));
    }
  }

  if(show_trace) {
    C_fputs(name, C_stderr);
    C_fputc('\n', C_stderr);
  }

  if(trace_buffer_top >= trace_buffer_limit) {
    trace_buffer_top = trace_buffer;
    trace_buffer_full = 1;
  }

  trace_buffer_top->raw = name;
  trace_buffer_top->cooked1 = C_SCHEME_FALSE;
  trace_buffer_top->cooked2 = C_SCHEME_FALSE;
  trace_buffer_top->thread = C_block_item(current_thread_symbol, 0);
  ++trace_buffer_top;
}


C_regparm C_word C_fcall C_emit_trace_info2(char *raw, C_word x, C_word y, C_word t)
{
  /* See above */
  if(profiling && next_profile_bucket == NULL) {
    next_profile_bucket = (PROFILE_BUCKET *)C_malloc(sizeof(PROFILE_BUCKET));
    if (next_profile_bucket == NULL) {
      panic(C_text("out of memory - cannot allocate profile table-bucket"));
    }
  }

  if(trace_buffer_top >= trace_buffer_limit) {
    trace_buffer_top = trace_buffer;
    trace_buffer_full = 1;
  }

  trace_buffer_top->raw = raw;
  trace_buffer_top->cooked1 = x;
  trace_buffer_top->cooked2 = y;
  trace_buffer_top->thread = t;
  ++trace_buffer_top;
  return x;
}


C_char *C_dump_trace(int start)
{
  TRACE_INFO *ptr;
  C_char *result;
  int i, result_len;

  result_len = STRING_BUFFER_SIZE;
  if((result = (char *)C_malloc(result_len)) == NULL)
    horror(C_text("out of memory - cannot allocate trace-dump buffer"));

  *result = '\0';

  if(trace_buffer_top > trace_buffer || trace_buffer_full) {
    if(trace_buffer_full) {
      i = C_trace_buffer_size;
      C_strlcat(result, C_text("...more...\n"), result_len);
    }
    else i = trace_buffer_top - trace_buffer;

    ptr = trace_buffer_full ? trace_buffer_top : trace_buffer;
    ptr += start;
    i -= start;

    for(;i--; ++ptr) {
      if(ptr >= trace_buffer_limit) ptr = trace_buffer;

      if(C_strlen(result) > STRING_BUFFER_SIZE - 32) {
        result_len = C_strlen(result) * 2;
        result = C_realloc(result, result_len);
	if(result == NULL)
	  horror(C_text("out of memory - cannot reallocate trace-dump buffer"));
      }

      C_strlcat(result, ptr->raw, result_len);

      if(i > 0) C_strlcat(result, "\n", result_len);
      else C_strlcat(result, " \t<--\n", result_len);
    }
  }

  return result;
}


C_regparm void C_fcall C_clear_trace_buffer(void)
{
  int i, old_profiling = profiling;

  profiling = 0;

  if(trace_buffer == NULL) {
    if(C_trace_buffer_size < MIN_TRACE_BUFFER_SIZE)
      C_trace_buffer_size = MIN_TRACE_BUFFER_SIZE;

    trace_buffer = (TRACE_INFO *)C_malloc(sizeof(TRACE_INFO) * C_trace_buffer_size);

    if(trace_buffer == NULL)
      panic(C_text("out of memory - cannot allocate trace-buffer"));
  }

  trace_buffer_top = trace_buffer;
  trace_buffer_limit = trace_buffer + C_trace_buffer_size;
  trace_buffer_full = 0;

  for(i = 0; i < C_trace_buffer_size; ++i) {
    trace_buffer[ i ].cooked1 = C_SCHEME_FALSE;
    trace_buffer[ i ].cooked2 = C_SCHEME_FALSE;
    trace_buffer[ i ].thread = C_SCHEME_FALSE;
  }

  profiling = old_profiling;
}

C_word C_resize_trace_buffer(C_word size) {
  int old_size = C_trace_buffer_size, old_profiling = profiling;
  assert(trace_buffer);
  profiling = 0;
  free(trace_buffer);
  trace_buffer = NULL;
  C_trace_buffer_size = C_unfix(size);
  C_clear_trace_buffer();
  profiling = old_profiling;
  return(C_fix(old_size));
}

C_word C_fetch_trace(C_word starti, C_word buffer)
{
  TRACE_INFO *ptr;
  int i, p = 0, start = C_unfix(starti);

  if(trace_buffer_top > trace_buffer || trace_buffer_full) {
    if(trace_buffer_full) i = C_trace_buffer_size;
    else i = trace_buffer_top - trace_buffer;

    ptr = trace_buffer_full ? trace_buffer_top : trace_buffer;
    ptr += start;
    i -= start;

    if(C_header_size(buffer) < i * 4)
      panic(C_text("destination buffer too small for call-chain"));

    for(;i--; ++ptr) {
      if(ptr >= trace_buffer_limit) ptr = trace_buffer;

      /* outside-pointer, will be ignored by GC */
      C_mutate2(&C_block_item(buffer, p++), (C_word)ptr->raw);

      /* subject to GC */
      C_mutate2(&C_block_item(buffer, p++), ptr->cooked1);
      C_mutate2(&C_block_item(buffer, p++), ptr->cooked2);
      C_mutate2(&C_block_item(buffer, p++), ptr->thread);
    }
  }

  return C_fix(p);
}

C_regparm C_word C_fcall C_u_i_string_hash(C_word str, C_word rnd)
{
  int len = C_header_size(str);
  C_char *ptr = C_data_pointer(str);
  return C_fix(hash_string(len, ptr, C_MOST_POSITIVE_FIXNUM, C_unfix(rnd), 0));
}

C_regparm C_word C_fcall C_u_i_string_ci_hash(C_word str, C_word rnd)
{
  int len = C_header_size(str);
  C_char *ptr = C_data_pointer(str);
  return C_fix(hash_string(len, ptr, C_MOST_POSITIVE_FIXNUM, C_unfix(rnd), 1));
}

C_regparm void C_fcall C_toplevel_entry(C_char *name)
{
  if(debug_mode)
    C_dbg(C_text("debug"), C_text("entering toplevel %s...\n"), name);
}


C_word C_halt(C_word msg)
{
  C_char *dmp = msg != C_SCHEME_FALSE ? C_dump_trace(0) : NULL;

  if(C_gui_mode) {
    if(msg != C_SCHEME_FALSE) {
      int n = C_header_size(msg);

      if (n >= sizeof(buffer))
	n = sizeof(buffer) - 1;
      C_strlcpy(buffer, (C_char *)C_data_pointer(msg), n);
      /* XXX msg isn't checked for NUL bytes, but we can't barf here either! */
    }
    else C_strlcpy(buffer, C_text("(aborted)"), sizeof(buffer));

    C_strlcat(buffer, C_text("\n\n"), sizeof(buffer));

    if(dmp != NULL) C_strlcat(buffer, dmp, sizeof(buffer));

#if defined(_WIN32) && !defined(__CYGWIN__) 
    MessageBox(NULL, buffer, C_text("CHICKEN runtime"), MB_OK | MB_ICONERROR);
    ExitProcess(1);
#endif
  } /* otherwise fall through */

  if(msg != C_SCHEME_FALSE) {
    C_fwrite(C_data_pointer(msg), C_header_size(msg), sizeof(C_char), C_stderr);
    C_fputc('\n', C_stderr);
  }

  if(dmp != NULL) 
    C_dbg("", C_text("\n%s"), dmp);
  
  C_exit_runtime(C_fix(EX_SOFTWARE));
  return 0;
}


C_word C_message(C_word msg)
{
  unsigned int n = C_header_size(msg);
  /*
   * Strictly speaking this isn't necessary for the non-gui-mode,
   * but let's try and keep this consistent across modes.
   */
  if (memchr(C_c_string(msg), '\0', n) != NULL)
    barf(C_ASCIIZ_REPRESENTATION_ERROR, "##sys#message", msg);

  if(C_gui_mode) {
    if (n >= sizeof(buffer))
      n = sizeof(buffer) - 1;
    C_strncpy(buffer, C_c_string(msg), n);
    buffer[ n ] = '\0';
#if defined(_WIN32) && !defined(__CYGWIN__)
    MessageBox(NULL, buffer, C_text("CHICKEN runtime"), MB_OK | MB_ICONEXCLAMATION);
    return C_SCHEME_UNDEFINED;
#endif
  } /* fall through */

  C_fwrite(C_c_string(msg), n, sizeof(C_char), stdout);
  C_putchar('\n');
  return C_SCHEME_UNDEFINED;
}


C_regparm C_word C_fcall C_equalp(C_word x, C_word y)
{
  C_header header;
  C_word bits, n, i;

  C_stack_check1(barf(C_CIRCULAR_DATA_ERROR, "equal?"));

 loop:
  if(x == y) return 1;

  if(C_immediatep(x) || C_immediatep(y)) return 0;

  if((header = C_block_header(x)) != C_block_header(y)) return 0;
  else if((bits = header & C_HEADER_BITS_MASK) & C_BYTEBLOCK_BIT) {
    if(header == C_FLONUM_TAG && C_block_header(y) == C_FLONUM_TAG)
      return C_flonum_magnitude(x) == C_flonum_magnitude(y);
    else return !C_memcmp(C_data_pointer(x), C_data_pointer(y), header & C_HEADER_SIZE_MASK);
  }
  else if(header == C_SYMBOL_TAG) return 0;
  else {
    i = 0;
    n = header & C_HEADER_SIZE_MASK;

    if(bits & C_SPECIALBLOCK_BIT) {
      /* do not recurse into closures */
      if(C_header_bits(x) == C_CLOSURE_TYPE)
	return !C_memcmp(C_data_pointer(x), C_data_pointer(y), n * sizeof(C_word));
      else if(C_block_item(x, 0) != C_block_item(y, 0)) return 0;
      else ++i;

      if(n == 1) return 1;
    }

    if(--n < 0) return 1;

    while(i < n)
      if(!C_equalp(C_block_item(x, i), C_block_item(y, i))) return 0;
      else ++i;

    x = C_block_item(x, i);
    y = C_block_item(y, i);
    goto loop;
  }
}


C_regparm C_word C_fcall C_set_gc_report(C_word flag)
{
  if(flag == C_SCHEME_FALSE) gc_report_flag = 0;
  else if(flag == C_SCHEME_TRUE) gc_report_flag = 2;
  else gc_report_flag = 1;

  return C_SCHEME_UNDEFINED;
}


C_regparm C_word C_fcall C_start_timer(void)
{
  tracked_mutation_count = 0;
  mutation_count = 0;
  gc_count_1_total = 0;
  gc_count_2 = 0;
  timer_start_ms = C_cpu_milliseconds();
  gc_ms = 0;
  maximum_heap_usage = 0;
  return C_SCHEME_UNDEFINED;
}


void C_ccall C_stop_timer(C_word c, C_word *av)
{
  C_word 
    closure = av[ 0 ],
    k = av[ 1 ];
  double t0 = C_cpu_milliseconds() - timer_start_ms;
  C_word 
    ab[ WORDS_PER_FLONUM * 3 + 8 ], /* 3 flonums, 1 vector of 7 elements */
    *a = ab,
    elapsed = C_flonum(&a, t0 / 1000.0),
    gc_time = C_flonum(&a, gc_ms / 1000.0),
    heap_usage = C_unsigned_int_to_num(&a, maximum_heap_usage),
    info;
  
  info = C_vector(&a, 7, elapsed, gc_time, C_fix(mutation_count),
                  C_fix(tracked_mutation_count), C_fix(gc_count_1_total),
		  C_fix(gc_count_2), heap_usage);
  C_kontinue(k, info);
}


C_word C_exit_runtime(C_word code)
{
  C_fflush(NULL);
  C__exit(C_unfix(code));
}


C_regparm C_word C_fcall C_set_print_precision(C_word n)
{
  flonum_print_precision = C_unfix(n);
  return C_SCHEME_UNDEFINED;
}


C_regparm C_word C_fcall C_get_print_precision(void)
{
  return C_fix(flonum_print_precision);
}


C_regparm C_word C_fcall C_read_char(C_word port)
{
  C_FILEPTR fp = C_port_file(port);
  int c = C_getc(fp);

  if(c == EOF) {
    if(ferror(fp)) {
      clearerr(fp);
      return C_fix(-1);
    }
    /* Found here:
       http://mail.python.org/pipermail/python-bugs-list/2002-July/012579.html */
#if defined(_WIN32) && !defined(__CYGWIN__)
    else if(GetLastError() == ERROR_OPERATION_ABORTED) return C_fix(-1);
#endif
    else return C_SCHEME_END_OF_FILE;
  }

  return C_make_character(c);
}


C_regparm C_word C_fcall C_peek_char(C_word port)
{
  C_FILEPTR fp = C_port_file(port);
  int c = C_getc(fp);

  if(c == EOF) {
    if(ferror(fp)) {
      clearerr(fp);
      return C_fix(-1);
    }
    /* see above */
#if defined(_WIN32) && !defined(__CYGWIN__)
    else if(GetLastError() == ERROR_OPERATION_ABORTED) return C_fix(-1);
#endif
    else return C_SCHEME_END_OF_FILE;
  }

  C_ungetc(c, fp);
  return C_make_character(c);
}


C_regparm C_word C_fcall C_execute_shell_command(C_word string)
{
  int n = C_header_size(string);
  char *buf = buffer;

  /* Windows doc says to flush all output streams before calling system.
     Probably a good idea for all platforms. */
  (void)fflush(NULL);

  if(n >= STRING_BUFFER_SIZE) {
    if((buf = (char *)C_malloc(n + 1)) == NULL)
      barf(C_OUT_OF_MEMORY_ERROR, "system");
  }

  C_memcpy(buf, C_data_pointer(string), n);
  buf[ n ] = '\0';
  if (n != strlen(buf))
    barf(C_ASCIIZ_REPRESENTATION_ERROR, "system", string);

  n = C_system(buf);

  if(buf != buffer) C_free(buf);

  return C_fix(n);
}

/*
 * TODO: Implement something for Windows that supports selecting on
 * arbitrary fds (there, select() only works on network sockets and
 * poll() is not available at all).
 */
C_regparm int C_fcall C_check_fd_ready(int fd)
{
#ifdef NO_POSIX_POLL
  fd_set in;
  struct timeval tm;
  int rv;
  FD_ZERO(&in);
  FD_SET(fd, &in);
  tm.tv_sec = tm.tv_usec = 0;
  rv = select(fd + 1, &in, NULL, NULL, &tm);
  if(rv > 0) { rv = FD_ISSET(fd, &in) ? 1 : 0; }
  return rv;
#else
  struct pollfd ps;
  ps.fd = fd;
  ps.events = POLLIN;
  return poll(&ps, 1, 0);
#endif
}

C_regparm C_word C_fcall C_char_ready_p(C_word port)
{
#if defined(C_NONUNIX)
  /* The best we can currently do on Windows... */
  return C_SCHEME_TRUE;
#else
  int fd = C_fileno(C_port_file(port));
  return C_mk_bool(C_check_fd_ready(fd) == 1);
#endif
}


C_regparm C_word C_fcall C_fudge(C_word fudge_factor)
{
  int i, j;
  double tgc;

  switch(fudge_factor) {
  case C_fix(1): return C_SCHEME_END_OF_FILE; /* eof object */
  case C_fix(2):			      /* get time */
    panic(C_text("(##sys#fudge 2) [get time] not implemented"));

  case C_fix(3):		/* 64-bit system? */
#ifdef C_SIXTY_FOUR
    return C_SCHEME_TRUE;
#else
    return C_SCHEME_FALSE;
#endif

  case C_fix(4):		/* is this a console application? */
    return C_mk_bool(!C_gui_mode);

  case C_fix(5):		/* is this a GUI/console or Windows-GUI application? (silly) */
    if(C_gui_mode) {
#ifdef _WIN32
      return C_fix(1);
#else
      return C_SCHEME_FALSE;
#endif
    }

    return C_fix(0);

  case C_fix(6): 		/* milliseconds CPU */
    panic(C_text("(##sys#fudge 6) [current CPU milliseconds] not implemented"));

  case C_fix(7):		/* wordsize */
    return C_fix(sizeof(C_word));

  case C_fix(8):		/* words needed for double */
    return C_fix(C_wordsperdouble(1));

  case C_fix(9):		/* latency */
    return C_fix(last_interrupt_latency);

  case C_fix(10):		/* clocks per sec */
    return C_fix(CLOCKS_PER_SEC);

  case C_fix(11):		/* not a unix system? */
#if defined(C_NONUNIX) || defined(__CYGWIN__)
    return C_SCHEME_FALSE;
#else
    return C_SCHEME_TRUE;
#endif

  case C_fix(12):		/* tty forced? */
    return C_mk_bool(fake_tty_flag);

  case C_fix(13):		/* debug mode */
    return C_mk_bool(debug_mode);

  case C_fix(14):		/* interrupts enabled? */
    return C_mk_bool(C_interrupts_enabled);

  case C_fix(15):		/* symbol-gc enabled? */
    return C_mk_bool(C_enable_gcweak);

  case C_fix(16):		/* milliseconds (wall clock) */
    panic(C_text("(##sys#fudge 16) [current wall clock milliseconds] not implemented"));

  case C_fix(17):		/* fixed heap? */
    return(C_mk_bool(C_heap_size_is_fixed));

  case C_fix(18):		/* stack direction */
    return(C_fix(C_STACK_GROWS_DOWNWARD));

  case C_fix(19):		/* number of locatives */
    for(i = j = 0; i < locative_table_count; ++i)
      if(locative_table[ i ] != C_SCHEME_UNDEFINED) ++j;
    return C_fix(j);

  case C_fix(20):		/* unused */
    return C_SCHEME_FALSE;

  case C_fix(21):		/* largest fixnum */
    return C_fix(C_MOST_POSITIVE_FIXNUM);

  case C_fix(22):		/* does this process use a private egg-repository? */
    return C_mk_bool(private_repository != NULL);

  case C_fix(23):		/* seconds since process startup */
    return C_fix(C_startup_time_seconds);

  case C_fix(24):		/* dynamic loading available? */
#ifdef NO_DLOAD2
    return C_SCHEME_FALSE;
#else
    return C_SCHEME_TRUE;
#endif

  case C_fix(25):		/* REPL on error? XXX Is this used anywhere? */
    return C_mk_bool(C_enable_repl);

  case C_fix(26):		/* number of untriggered finalizers */
    return C_fix(live_finalizer_count);

  case C_fix(27):		/* total number of finalizers used and unused */
    return C_fix(allocated_finalizer_count);

  case C_fix(28):		/* are procedure-tabled enabled? */
#ifdef C_ENABLE_PTABLES
    return C_SCHEME_TRUE;
#else
    return C_SCHEME_FALSE;
#endif

  case C_fix(29):		/* size of ring-buffer used to hold trace entries */
    return C_fix(C_trace_buffer_size);

  case C_fix(30):		/* unused */
    return C_SCHEME_FALSE;

  case C_fix(31):		/* GC time since last invocation */
    tgc = timer_accumulated_gc_ms;
    timer_accumulated_gc_ms = 0;
    return C_fix(tgc);

  case C_fix(32):		/* are GC-hooks enabled? */
#ifdef C_GC_HOOKS
    return C_SCHEME_TRUE;
#else
    return C_SCHEME_FALSE;
#endif

  case C_fix(33):		/* return process-ID */
    return C_fix(C_getpid());

  case C_fix(34):		/* effective maximum for procedure arguments */
    return C_fix(stack_size / 2); /* An educated guess :) */

  case C_fix(35):		/* unused */
    /* used to be apply-hook indicator */
    return C_SCHEME_FALSE;
    
  case C_fix(36):		/* toggle debug mode */
    debug_mode = !debug_mode;
    return C_mk_bool(debug_mode);

  case C_fix(37):		/* heap-dump enabled? */
    return C_mk_bool(dump_heap_on_exit);

  case C_fix(38):		/* SVN revision of built sources */
#ifdef C_SVN_REVISION
    return C_fix(C_SVN_REVISION);
#else
    return C_fix(0);
#endif

  case C_fix(39):		/* is this a cross-chicken? */
#if defined(C_CROSS_CHICKEN) && C_CROSS_CHICKEN
    return C_SCHEME_TRUE;
#else
    return C_SCHEME_FALSE;
#endif

  case C_fix(40):		/* many arguments supported? */
    return C_SCHEME_TRUE;

  case C_fix(41):		/* major CHICKEN version */
    return C_fix(C_MAJOR_VERSION);

  case C_fix(42):		/* binary version number */
#ifdef C_BINARY_VERSION
    return C_fix(C_BINARY_VERSION);
#else
    return C_fix(0);
#endif

  case C_fix(43):		/* minor CHICKEN version */
    return C_fix(C_MINOR_VERSION);

  case C_fix(44):  /* whether debugger is active */
    return C_mk_bool(C_debugging);

  case C_fix(45):  /* Whether we're currently profiling */
    return C_mk_bool(profiling);

  default: return C_SCHEME_UNDEFINED;
  }
}


C_regparm void C_fcall C_paranoid_check_for_interrupt(void)
{
  if(--C_timer_interrupt_counter <= 0)
    C_raise_interrupt(C_TIMER_INTERRUPT_NUMBER);
}


C_regparm void C_fcall C_raise_interrupt(int reason)
{
  if(C_interrupts_enabled) {
    if(pending_interrupts_count == 0 && !handling_interrupts) {
      pending_interrupts[ pending_interrupts_count++ ] = reason;
      /*
       * Force the next "soft" stack check to fail by faking a "full"
       * stack.  This causes save_and_reclaim() to be called, which
       * invokes handle_interrupt(), which restores the stack limit.
       */
      C_stack_limit = stack_bottom;
      interrupt_time = C_cpu_milliseconds();
    } else if(pending_interrupts_count < MAX_PENDING_INTERRUPTS) {
      int i;
      /*
       * Drop signals if too many, but don't queue up multiple entries
       * for the same signal.
       */
      for (i = 0; i < pending_interrupts_count; ++i) {
        if (pending_interrupts[i] == reason)
          return;
      }
      pending_interrupts[ pending_interrupts_count++ ] = reason;
    }
  }
}


C_regparm C_word C_fcall C_enable_interrupts(void)
{
  C_timer_interrupt_counter = C_initial_timer_interrupt_period;
  /* assert(C_timer_interrupt_counter > 0); */
  C_interrupts_enabled = 1;
  return C_SCHEME_UNDEFINED;
}


C_regparm C_word C_fcall C_disable_interrupts(void)
{
  C_interrupts_enabled = 0;
  return C_SCHEME_UNDEFINED;
}


C_regparm C_word C_fcall C_establish_signal_handler(C_word signum, C_word reason)
{
  int sig = C_unfix(signum);
#if defined(HAVE_SIGACTION)
  struct sigaction newsig;
#endif

  if(reason == C_SCHEME_FALSE) C_signal(sig, SIG_IGN);
  else {
    signal_mapping_table[ sig ] = C_unfix(reason);
#if defined(HAVE_SIGACTION)
    newsig.sa_flags = 0;
    /* The global signal handler is used for all signals, and
       manipulates a single queue.  Don't allow other signals to
       concurrently arrive while it's doing this, to avoid races. */
    sigfillset(&newsig.sa_mask);
    newsig.sa_handler = global_signal_handler;
    C_sigaction(sig, &newsig, NULL);
#else
    C_signal(sig, global_signal_handler);
#endif
  }

  return C_SCHEME_UNDEFINED;
}


/* Copy blocks into collected or static memory: */

C_regparm C_word C_fcall C_copy_block(C_word from, C_word to)
{
  int n = C_header_size(from);
  C_long bytes;

  if(C_header_bits(from) & C_BYTEBLOCK_BIT) {
    bytes = n;
    C_memcpy((C_SCHEME_BLOCK *)to, (C_SCHEME_BLOCK *)from, bytes + sizeof(C_header));
  }
  else {
    bytes = C_wordstobytes(n);
    C_memcpy((C_SCHEME_BLOCK *)to, (C_SCHEME_BLOCK *)from, bytes + sizeof(C_header));
  }

  return to;
}


C_regparm C_word C_fcall C_evict_block(C_word from, C_word ptr)
{
  int n = C_header_size(from);
  C_long bytes;
  C_word *p = (C_word *)C_pointer_address(ptr);

  if(C_header_bits(from) & C_BYTEBLOCK_BIT) bytes = n;
  else bytes = C_wordstobytes(n);

  C_memcpy(p, (C_SCHEME_BLOCK *)from, bytes + sizeof(C_header));
  return (C_word)p;
}


/* Inline versions of some standard procedures: */

C_regparm C_word C_fcall C_i_listp(C_word x)
{
  C_word fast = x, slow = x;

  while(fast != C_SCHEME_END_OF_LIST)
    if(!C_immediatep(fast) && C_block_header(fast) == C_PAIR_TAG) {
      fast = C_u_i_cdr(fast);
      
      if(fast == C_SCHEME_END_OF_LIST) return C_SCHEME_TRUE;
      else if(!C_immediatep(fast) && C_block_header(fast) == C_PAIR_TAG) {
	fast = C_u_i_cdr(fast);
	slow = C_u_i_cdr(slow);

	if(fast == slow) return C_SCHEME_FALSE;
      }
      else return C_SCHEME_FALSE;
    }
    else return C_SCHEME_FALSE;

  return C_SCHEME_TRUE;
}


C_regparm C_word C_fcall C_i_string_equal_p(C_word x, C_word y)
{
  C_word n;

  if(C_immediatep(x) || C_header_bits(x) != C_STRING_TYPE)
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "string=?", x);

  if(C_immediatep(y) || C_header_bits(y) != C_STRING_TYPE)
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "string=?", y);

  n = C_header_size(x);

  return C_mk_bool(n == C_header_size(y)
                   && !C_memcmp((char *)C_data_pointer(x), (char *)C_data_pointer(y), n));
}


C_regparm C_word C_fcall C_i_string_ci_equal_p(C_word x, C_word y)
{
  C_word n;
  char *p1, *p2;

  if(C_immediatep(x) || C_header_bits(x) != C_STRING_TYPE)
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "string-ci=?", x);

  if(C_immediatep(y) || C_header_bits(y) != C_STRING_TYPE)
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "string-ci=?", y);

  n = C_header_size(x);

  if(n != C_header_size(y)) return C_SCHEME_FALSE;

  p1 = (char *)C_data_pointer(x);
  p2 = (char *)C_data_pointer(y);

  while(n--) {
    if(C_tolower((int)(*(p1++))) != C_tolower((int)(*(p2++))))
      return C_SCHEME_FALSE;
  }

  return C_SCHEME_TRUE;
}


C_word C_a_i_list(C_word **a, int c, ...)
{
  va_list v;
  C_word x, last, current,
         first = C_SCHEME_END_OF_LIST;

  va_start(v, c);

  for(last = C_SCHEME_UNDEFINED; c--; last = current) {
    x = va_arg(v, C_word);
    current = C_a_pair(a, x, C_SCHEME_END_OF_LIST);

    if(last != C_SCHEME_UNDEFINED)
      C_set_block_item(last, 1, current);
    else first = current;
  }

  va_end(v);
  return first;
}


C_word C_a_i_string(C_word **a, int c, ...)
{
  va_list v;
  C_word x, s = (C_word)(*a);
  char *p;

  *a = (C_word *)((C_word)(*a) + sizeof(C_header) + C_align(c));
  C_block_header_init(s, C_STRING_TYPE | c);
  p = (char *)C_data_pointer(s);
  va_start(v, c);

  for(; c; c--) {
    x = va_arg(v, C_word);

    if((x & C_IMMEDIATE_TYPE_BITS) == C_CHARACTER_BITS)
      *(p++) = C_character_code(x);
    else break;
  }

  va_end(v);
  if (c) barf(C_BAD_ARGUMENT_TYPE_ERROR, "string", x);
  return s;
}


C_word C_a_i_record(C_word **ptr, int n, ...)
{
  va_list v;
  C_word *p = *ptr,
         *p0 = p; 

  *(p++) = C_STRUCTURE_TYPE | n;
  va_start(v, n);

  while(n--)
    *(p++) = va_arg(v, C_word);

  *ptr = p;
  va_end(v);
  return (C_word)p0;
}


C_word C_a_i_port(C_word **ptr, int n)
{
  C_word 
    *p = *ptr,
    *p0 = p; 
  int i;

  *(p++) = C_PORT_TYPE | (C_SIZEOF_PORT - 1);
  *(p++) = (C_word)NULL;
  
  for(i = 0; i < C_SIZEOF_PORT - 2; ++i)
    *(p++) = C_SCHEME_FALSE;

  *ptr = p;
  return (C_word)p0;
}


C_regparm C_word C_fcall C_a_i_bytevector(C_word **ptr, int c, C_word num)
{
  C_word *p = *ptr,
         *p0;
  int n = C_unfix(num);

#ifndef C_SIXTY_FOUR
  /* Align on 8-byte boundary: */
  if(C_aligned8(p)) ++p;
#endif

  p0 = p;
  *(p++) = C_BYTEVECTOR_TYPE | C_wordstobytes(n);
  *ptr = p + n;
  return (C_word)p0;
}


C_word C_fcall C_a_i_smart_mpointer(C_word **ptr, int c, C_word x)
{
  C_word 
    *p = *ptr,
    *p0 = p;
  void *mp;

  if(C_immediatep(x)) mp = NULL;
  else if((C_header_bits(x) & C_SPECIALBLOCK_BIT) != 0) mp = C_pointer_address(x);
  else mp = C_data_pointer(x);

  *(p++) = C_POINTER_TYPE | 1;
  *((void **)p) = mp;
  *ptr = p + 1;
  return (C_word)p0;
}


C_regparm C_word C_fcall C_i_exactp(C_word x)
{
  if(x & C_FIXNUM_BIT) return C_SCHEME_TRUE;

  if(C_immediatep(x) || C_block_header(x) != C_FLONUM_TAG)
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "exact?", x);

  return C_SCHEME_FALSE;
}


C_regparm C_word C_fcall C_i_inexactp(C_word x)
{
  if(x & C_FIXNUM_BIT) return C_SCHEME_FALSE;

  if(C_immediatep(x) || C_block_header(x) != C_FLONUM_TAG)
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "inexact?", x);

  return C_SCHEME_TRUE;
}


C_regparm C_word C_fcall C_i_zerop(C_word x)
{
  if(x & C_FIXNUM_BIT) return C_mk_bool(x == C_fix(0));

  if(C_immediatep(x) || C_block_header(x) != C_FLONUM_TAG)
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "zero?", x);

  return C_mk_bool(C_flonum_magnitude(x) == 0.0);
}


/* I */
C_regparm C_word C_fcall C_u_i_zerop(C_word x)
{
  if(x & C_FIXNUM_BIT) return C_mk_bool(x == C_fix(0));

  return C_mk_bool(C_flonum_magnitude(x) == 0.0);
}


C_regparm C_word C_fcall C_i_positivep(C_word x)
{
  if(x & C_FIXNUM_BIT) return C_mk_bool(C_unfix(x) > 0);

  if(C_immediatep(x) || C_block_header(x) != C_FLONUM_TAG)
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "positive?", x);

  return C_mk_bool(C_flonum_magnitude(x) > 0.0);
}


/* I */
C_regparm C_word C_fcall C_u_i_positivep(C_word x)
{
  if(x & C_FIXNUM_BIT) return C_mk_bool(C_unfix(x) > 0);

  return C_mk_bool(C_flonum_magnitude(x) > 0.0);
}


C_regparm C_word C_fcall C_i_negativep(C_word x)
{
  if(x & C_FIXNUM_BIT) return C_mk_bool(C_unfix(x) < 0);

  if(C_immediatep(x) || C_block_header(x) != C_FLONUM_TAG)
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "negative?", x);

  return C_mk_bool(C_flonum_magnitude(x) < 0.0);
}


/* I */
C_regparm C_word C_fcall C_u_i_negativep(C_word x)
{
  if(x & C_FIXNUM_BIT) return C_mk_bool(C_unfix(x) < 0);

  return C_mk_bool(C_flonum_magnitude(x) < 0.0);
}


C_regparm C_word C_fcall C_i_evenp(C_word x)
{
  double val, dummy;
  if(x & C_FIXNUM_BIT) return C_mk_nbool(x & 0x02);

  if(C_immediatep(x) || C_block_header(x) != C_FLONUM_TAG)
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "even?", x);

  val = C_flonum_magnitude(x);
  if(C_isnan(val) || C_isinf(val) || C_modf(val, &dummy) != 0.0)
    barf(C_BAD_ARGUMENT_TYPE_NO_INTEGER_ERROR, "even?", x);

  return C_mk_bool(fmod(val, 2.0) == 0.0);
}


/* I */
C_regparm C_word C_fcall C_u_i_evenp(C_word x)
{
  if(x & C_FIXNUM_BIT) return C_mk_nbool(x & 0x02);

  return C_mk_bool(fmod(C_flonum_magnitude(x), 2.0) == 0.0);
}


C_regparm C_word C_fcall C_i_oddp(C_word x)
{
  double val, dummy;
  if(x & C_FIXNUM_BIT) return C_mk_bool(x & 0x02);

  if(C_immediatep(x) || C_block_header(x) != C_FLONUM_TAG)
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "odd?", x);

  val = C_flonum_magnitude(x);
  if(C_isnan(val) || C_isinf(val) || C_modf(val, &dummy) != 0.0)
    barf(C_BAD_ARGUMENT_TYPE_NO_INTEGER_ERROR, "odd?", x);

  return C_mk_bool(fmod(val, 2.0) != 0.0);
}


/* I */
C_regparm C_word C_fcall C_u_i_oddp(C_word x)
{
  if(x & C_FIXNUM_BIT) return C_mk_bool(x & 0x02);

  return C_mk_bool(fmod(C_flonum_magnitude(x), 2.0) != 0.0);
}


C_regparm C_word C_fcall C_i_car(C_word x)
{
  if(C_immediatep(x) || C_block_header(x) != C_PAIR_TAG)
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "car", x);

  return C_u_i_car(x);
}


C_regparm C_word C_fcall C_i_cdr(C_word x)
{
  if(C_immediatep(x) || C_block_header(x) != C_PAIR_TAG)
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "cdr", x);

  return C_u_i_cdr(x);
}


C_regparm C_word C_fcall C_i_caar(C_word x)
{
  if(C_immediatep(x) || C_block_header(x) != C_PAIR_TAG) {
  bad:
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "caar", x);
  }

  x = C_u_i_car(x);

  if(C_immediatep(x) || C_block_header(x) != C_PAIR_TAG) goto bad;

  return C_u_i_car(x);
}


C_regparm C_word C_fcall C_i_cadr(C_word x)
{
  if(C_immediatep(x) || C_block_header(x) != C_PAIR_TAG) {
  bad:
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "cadr", x);
  }

  x = C_u_i_cdr(x);

  if(C_immediatep(x) || C_block_header(x) != C_PAIR_TAG) goto bad;

  return C_u_i_car(x);
}


C_regparm C_word C_fcall C_i_cdar(C_word x)
{
  if(C_immediatep(x) || C_block_header(x) != C_PAIR_TAG) {
  bad:
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "cdar", x);
  }

  x = C_u_i_car(x);

  if(C_immediatep(x) || C_block_header(x) != C_PAIR_TAG) goto bad;

  return C_u_i_cdr(x);
}


C_regparm C_word C_fcall C_i_cddr(C_word x)
{
  if(C_immediatep(x) || C_block_header(x) != C_PAIR_TAG) {
  bad:
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "cddr", x);
  }

  x = C_u_i_cdr(x);
  if(C_immediatep(x) || C_block_header(x) != C_PAIR_TAG) goto bad;

  return C_u_i_cdr(x);
}


C_regparm C_word C_fcall C_i_caddr(C_word x)
{
  if(C_immediatep(x) || C_block_header(x) != C_PAIR_TAG) {
  bad:
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "caddr", x);
  }

  x = C_u_i_cdr(x);
  if(C_immediatep(x) || C_block_header(x) != C_PAIR_TAG) goto bad;
  x = C_u_i_cdr(x);
  if(C_immediatep(x) || C_block_header(x) != C_PAIR_TAG) goto bad;

  return C_u_i_car(x);
}


C_regparm C_word C_fcall C_i_cdddr(C_word x)
{
  if(C_immediatep(x) || C_block_header(x) != C_PAIR_TAG) {
  bad:
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "cdddr", x);
  }

  x = C_u_i_cdr(x);
  if(C_immediatep(x) || C_block_header(x) != C_PAIR_TAG) goto bad;
  x = C_u_i_cdr(x);
  if(C_immediatep(x) || C_block_header(x) != C_PAIR_TAG) goto bad;

  return C_u_i_cdr(x);
}


C_regparm C_word C_fcall C_i_cadddr(C_word x)
{
  if(C_immediatep(x) || C_block_header(x) != C_PAIR_TAG) {
  bad:
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "cadddr", x);
  }

  x = C_u_i_cdr(x);
  if(C_immediatep(x) || C_block_header(x) != C_PAIR_TAG) goto bad;
  x = C_u_i_cdr(x);
  if(C_immediatep(x) || C_block_header(x) != C_PAIR_TAG) goto bad;
  x = C_u_i_cdr(x);
  if(C_immediatep(x) || C_block_header(x) != C_PAIR_TAG) goto bad;

  return C_u_i_car(x);
}


C_regparm C_word C_fcall C_i_cddddr(C_word x)
{
  if(C_immediatep(x) || C_block_header(x) != C_PAIR_TAG) {
  bad:
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "cddddr", x);
  }

  x = C_u_i_cdr(x);
  if(C_immediatep(x) || C_block_header(x) != C_PAIR_TAG) goto bad;
  x = C_u_i_cdr(x);
  if(C_immediatep(x) || C_block_header(x) != C_PAIR_TAG) goto bad;
  x = C_u_i_cdr(x);
  if(C_immediatep(x) || C_block_header(x) != C_PAIR_TAG) goto bad;

  return C_u_i_cdr(x);
}


C_regparm C_word C_fcall C_i_list_tail(C_word lst, C_word i)
{
  C_word lst0 = lst;
  int n;

  if(lst != C_SCHEME_END_OF_LIST && 
     (C_immediatep(lst) || C_block_header(lst) != C_PAIR_TAG))
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "list-tail", lst);

  if(i & C_FIXNUM_BIT) n = C_unfix(i);
  else barf(C_BAD_ARGUMENT_TYPE_ERROR, "list-tail", i);

  while(n--) {
    if(C_immediatep(lst) || C_block_header(lst) != C_PAIR_TAG)
      barf(C_OUT_OF_RANGE_ERROR, "list-tail", lst0, i);
    
    lst = C_u_i_cdr(lst);
  }

  return lst;
}


C_regparm C_word C_fcall C_i_vector_ref(C_word v, C_word i)
{
  int j;

  if(C_immediatep(v) || C_header_bits(v) != C_VECTOR_TYPE)
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "vector-ref", v);

  if(i & C_FIXNUM_BIT) {
    j = C_unfix(i);

    if(j < 0 || j >= C_header_size(v)) barf(C_OUT_OF_RANGE_ERROR, "vector-ref", v, i);

    return C_block_item(v, j);
  }
  
  barf(C_BAD_ARGUMENT_TYPE_ERROR, "vector-ref", i);
  return C_SCHEME_UNDEFINED;
}


C_regparm C_word C_fcall C_i_block_ref(C_word x, C_word i)
{
  int j;

  if(C_immediatep(x) || (C_header_bits(x) & C_BYTEBLOCK_BIT) != 0)
    barf(C_BAD_ARGUMENT_TYPE_NO_BLOCK_ERROR, "##sys#block-ref", x);

  if(i & C_FIXNUM_BIT) {
    j = C_unfix(i);

    if(j < 0 || j >= C_header_size(x)) barf(C_OUT_OF_RANGE_ERROR, "##sys#block-ref", x, i);

    return C_block_item(x, j);
  }
  
  barf(C_BAD_ARGUMENT_TYPE_ERROR, "##sys#block-ref", i);
  return C_SCHEME_UNDEFINED;
}


C_regparm C_word C_fcall C_i_string_set(C_word s, C_word i, C_word c)
{
  int j;

  if(C_immediatep(s) || C_header_bits(s) != C_STRING_TYPE)
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "string-set!", s);

  if(!C_immediatep(c) || (c & C_IMMEDIATE_TYPE_BITS) != C_CHARACTER_BITS)
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "string-set!", c);

  if(i & C_FIXNUM_BIT) {
    j = C_unfix(i);

    if(j < 0 || j >= C_header_size(s)) barf(C_OUT_OF_RANGE_ERROR, "string-set!", s, i);

    return C_setsubchar(s, i, c);
  }

  barf(C_BAD_ARGUMENT_TYPE_ERROR, "string-set!", i);
  return C_SCHEME_UNDEFINED;
}


C_regparm C_word C_fcall C_i_string_ref(C_word s, C_word i)
{
  int j;

  if(C_immediatep(s) || C_header_bits(s) != C_STRING_TYPE)
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "string-ref", s);

  if(i & C_FIXNUM_BIT) {
    j = C_unfix(i);

    if(j < 0 || j >= C_header_size(s)) barf(C_OUT_OF_RANGE_ERROR, "string-ref", s, i);

    return C_subchar(s, i);
  }
  
  barf(C_BAD_ARGUMENT_TYPE_ERROR, "string-ref", i);
  return C_SCHEME_UNDEFINED;
}


C_regparm C_word C_fcall C_i_vector_length(C_word v)
{
  if(C_immediatep(v) || C_header_bits(v) != C_VECTOR_TYPE)
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "vector-length", v);

  return C_fix(C_header_size(v));
}


C_regparm C_word C_fcall C_i_string_length(C_word s)
{
  if(C_immediatep(s) || C_header_bits(s) != C_STRING_TYPE)
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "string-length", s);

  return C_fix(C_header_size(s));
}


C_regparm C_word C_fcall C_i_length(C_word lst)
{
  C_word fast = lst, slow = lst;
  int n = 0;

  while(slow != C_SCHEME_END_OF_LIST) {
    if(fast != C_SCHEME_END_OF_LIST) {
      if(!C_immediatep(fast) && C_block_header(fast) == C_PAIR_TAG) {
	fast = C_u_i_cdr(fast);
      
	if(fast != C_SCHEME_END_OF_LIST) {
	  if(!C_immediatep(fast) && C_block_header(fast) == C_PAIR_TAG) {
	    fast = C_u_i_cdr(fast);
	  }
	  else barf(C_NOT_A_PROPER_LIST_ERROR, "length", lst);
	}

	if(fast == slow) 
	  barf(C_BAD_ARGUMENT_TYPE_CYCLIC_LIST_ERROR, "length", lst);
      }
    }

    if(C_immediatep(slow) || C_block_header(slow) != C_PAIR_TAG)
      barf(C_NOT_A_PROPER_LIST_ERROR, "length", lst);

    slow = C_u_i_cdr(slow);
    ++n;
  }

  return C_fix(n);
}


C_regparm C_word C_fcall C_u_i_length(C_word lst)
{
  int n = 0;

  while(!C_immediatep(lst) && C_block_header(lst) == C_PAIR_TAG) {
    lst = C_u_i_cdr(lst);
    ++n;
  }

  return C_fix(n);
}

C_regparm C_word maybe_inexact_to_exact(C_word n)
{
  double m;
  C_word r;
  
  if(modf(C_flonum_magnitude(n), &m) == 0.0) {
    r = (C_word)m;
    
    if(r == m && C_fitsinfixnump(r))
      return C_fix(r);
  }
  return C_SCHEME_FALSE;
}

C_regparm C_word C_fcall C_i_inexact_to_exact(C_word n)
{
  C_word r;
  
  if(n & C_FIXNUM_BIT) return n;
  else if(C_immediatep(n) || C_block_header(n) != C_FLONUM_TAG)
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "inexact->exact", n);

  r = maybe_inexact_to_exact(n);
  if (r != C_SCHEME_FALSE) return r;

  barf(C_CANT_REPRESENT_INEXACT_ERROR, "inexact->exact", n);
  return 0;
}


C_regparm C_word C_fcall C_i_set_car(C_word x, C_word val)
{
  if(C_immediatep(x) || C_block_header(x) != C_PAIR_TAG)
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "set-car!", x);

  C_mutate2(&C_u_i_car(x), val);
  return C_SCHEME_UNDEFINED;
}


C_regparm C_word C_fcall C_i_set_cdr(C_word x, C_word val)
{
  if(C_immediatep(x) || C_block_header(x) != C_PAIR_TAG)
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "set-cdr!", x);

  C_mutate2(&C_u_i_cdr(x), val);
  return C_SCHEME_UNDEFINED;
}


C_regparm C_word C_fcall C_i_vector_set(C_word v, C_word i, C_word x)
{
  int j;

  if(C_immediatep(v) || C_header_bits(v) != C_VECTOR_TYPE)
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "vector-set!", v);

  if(i & C_FIXNUM_BIT) {
    j = C_unfix(i);

    if(j < 0 || j >= C_header_size(v)) barf(C_OUT_OF_RANGE_ERROR, "vector-set!", v, i);

    C_mutate2(&C_block_item(v, j), x);
  }
  else barf(C_BAD_ARGUMENT_TYPE_ERROR, "vector-set!", i);

  return C_SCHEME_UNDEFINED;
}


C_regparm C_word C_fcall C_a_i_abs(C_word **a, int c, C_word x)
{
  if(x & C_FIXNUM_BIT) return C_fix(labs(C_unfix(x)));

  if(C_immediatep(x) || C_block_header(x) != C_FLONUM_TAG)
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "abs", x);

  return C_flonum(a, fabs(C_flonum_magnitude(x)));
}


C_regparm C_word C_fcall C_a_i_bitwise_and(C_word **a, int c, C_word n1, C_word n2)
{
  double f1, f2;
  C_uword nn1, nn2;

  C_check_uint(n1, f1, nn1, "bitwise-and");
  C_check_uint(n2, f2, nn2, "bitwise-and");
  nn1 = C_limit_fixnum(nn1 & nn2);

  if(C_ufitsinfixnump(nn1)) return C_fix(nn1);
  else return C_flonum(a, nn1);
}


C_regparm C_word C_fcall C_a_i_bitwise_ior(C_word **a, int c, C_word n1, C_word n2)
{
  double f1, f2;
  C_uword nn1, nn2;

  C_check_uint(n1, f1, nn1, "bitwise-ior");
  C_check_uint(n2, f2, nn2, "bitwise-ior");
  nn1 = C_limit_fixnum(nn1 | nn2);

  if(C_ufitsinfixnump(nn1)) return C_fix(nn1);
  else return C_flonum(a, nn1);
}


C_regparm C_word C_fcall C_a_i_bitwise_xor(C_word **a, int c, C_word n1, C_word n2)
{
  double f1, f2;
  C_uword nn1, nn2;

  C_check_uint(n1, f1, nn1, "bitwise-xor");
  C_check_uint(n2, f2, nn2, "bitwise-xor");
  nn1 = C_limit_fixnum(nn1 ^ nn2);

  if(C_ufitsinfixnump(nn1)) return C_fix(nn1);
  else return C_flonum(a, nn1);
}


C_regparm C_word C_fcall C_i_bit_setp(C_word n, C_word i)
{
  double f1;
  C_uword nn1;
  int index;

  if((i & C_FIXNUM_BIT) == 0) 
    barf(C_BAD_ARGUMENT_TYPE_NO_FIXNUM_ERROR, "bit-set?", i);

  index = C_unfix(i);

  if(index < 0 || index >= C_WORD_SIZE)
    barf(C_OUT_OF_RANGE_ERROR, "bit-set?", n, i);

  C_check_uint(n, f1, nn1, "bit-set?");
  return C_mk_bool((nn1 & (1 << index)) != 0);
}


C_regparm C_word C_fcall C_a_i_bitwise_not(C_word **a, int c, C_word n)
{
  double f;
  C_uword nn;

  C_check_uint(n, f, nn, "bitwise-not");
  nn = C_limit_fixnum(~nn);

  if(C_ufitsinfixnump(nn)) return C_fix(nn);
  else return C_flonum(a, nn);
}


C_regparm C_word C_fcall C_a_i_arithmetic_shift(C_word **a, int c, C_word n1, C_word n2)
{
  C_word nn;
  C_uword unn;
  C_word s;
  int sgn = 1;

  if((n1 & C_FIXNUM_BIT) != 0) {
    nn = C_unfix(n1);

    if((sgn = nn < 0 ? -1 : 1) >= 0) unn = nn;
  }
  else if(C_immediatep(n1) || C_block_header(n1) != C_FLONUM_TAG)
    barf(C_BAD_ARGUMENT_TYPE_NO_NUMBER_ERROR, "arithmetic-shift", n1);
  else { 
    double m, f;

    f = C_flonum_magnitude(n1);
    
    if(C_isnan(f) || C_isinf(f) || C_modf(f, &m) != 0.0)
      barf(C_BAD_ARGUMENT_TYPE_NO_INTEGER_ERROR, "arithmetic-shift", n1);

    if(f < C_WORD_MIN || f > C_UWORD_MAX)
      barf(C_BAD_ARGUMENT_TYPE_NO_INTEGER_ERROR, "arithmetic-shift", n1);
    else if(f < 0) {
      if(f > C_WORD_MAX)
	barf(C_BAD_ARGUMENT_TYPE_NO_INTEGER_ERROR, "arithmetic-shift", n1);
      else {
	sgn = -1;
	nn = (C_word)f;
      }
    }
    else if(f > C_WORD_MAX) unn = (C_uword)f;
    else {
      nn = (C_word)f;
      sgn = -1;
    }
  }

  if((n2 & C_FIXNUM_BIT) != 0) s = C_unfix(n2);
  else barf(C_BAD_ARGUMENT_TYPE_NO_FIXNUM_ERROR, "arithmetic-shift", n2);

  if(sgn < 0) {
    if(s < 0) nn >>= -s;
    else nn = (C_word)((C_uword)nn << s);

    if(C_fitsinfixnump(nn)) return C_fix(nn);
    else return C_flonum(a, nn);
  } 
  else {
    if(s < 0) unn >>= -s;
    else unn <<= s;
  
    if(C_ufitsinfixnump(unn)) return C_fix(unn);
    else return C_flonum(a, unn);
  }
}


/* I */
C_regparm C_word C_fcall C_a_i_exp(C_word **a, int c, C_word n)
{
  double f;

  C_check_real(n, "exp", f);
  return C_flonum(a, exp(f));
}


/* I */
C_regparm C_word C_fcall C_a_i_log(C_word **a, int c, C_word n)
{
  double f;

  C_check_real(n, "log", f);
  return C_flonum(a, log(f));
}


/* I */
C_regparm C_word C_fcall C_a_i_sin(C_word **a, int c, C_word n)
{
  double f;

  C_check_real(n, "sin", f);
  return C_flonum(a, sin(f));
}


/* I */
C_regparm C_word C_fcall C_a_i_cos(C_word **a, int c, C_word n)
{
  double f;

  C_check_real(n, "cos", f);
  return C_flonum(a, cos(f));
}


/* I */
C_regparm C_word C_fcall C_a_i_tan(C_word **a, int c, C_word n)
{
  double f;

  C_check_real(n, "tan", f);
  return C_flonum(a, tan(f));
}


/* I */
C_regparm C_word C_fcall C_a_i_asin(C_word **a, int c, C_word n)
{
  double f;

  C_check_real(n, "asin", f);
  return C_flonum(a, asin(f));
}


/* I */
C_regparm C_word C_fcall C_a_i_acos(C_word **a, int c, C_word n)
{
  double f;

  C_check_real(n, "acos", f);
  return C_flonum(a, acos(f));
}


/* I */
C_regparm C_word C_fcall C_a_i_atan(C_word **a, int c, C_word n)
{
  double f;

  C_check_real(n, "atan", f);
  return C_flonum(a, atan(f));
}


/* I */
C_regparm C_word C_fcall C_a_i_atan2(C_word **a, int c, C_word n1, C_word n2)
{
  double f1, f2;

  C_check_real(n1, "atan", f1);
  C_check_real(n2, "atan", f2);
  return C_flonum(a, atan2(f1, f2));
}


/* I */
C_regparm C_word C_fcall C_a_i_sqrt(C_word **a, int c, C_word n)
{
  double f;

  C_check_real(n, "sqrt", f);
  return C_flonum(a, sqrt(f));
}


C_regparm C_word C_fcall C_i_assq(C_word x, C_word lst)
{
  C_word a;

  while(!C_immediatep(lst) && C_block_header(lst) == C_PAIR_TAG) {
    a = C_u_i_car(lst);

    if(!C_immediatep(a) && C_block_header(a) == C_PAIR_TAG) {
      if(C_u_i_car(a) == x) return a;
    }
    else barf(C_BAD_ARGUMENT_TYPE_ERROR, "assq", a);
  
    lst = C_u_i_cdr(lst);
  }

  if(lst!=C_SCHEME_END_OF_LIST)
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "assq", lst);

  return C_SCHEME_FALSE;
}


C_regparm C_word C_fcall C_i_assv(C_word x, C_word lst)
{
  C_word a;

  while(!C_immediatep(lst) && C_block_header(lst) == C_PAIR_TAG) {
    a = C_u_i_car(lst);

    if(!C_immediatep(a) && C_block_header(a) == C_PAIR_TAG) {
      if(C_truep(C_i_eqvp(C_u_i_car(a), x))) return a;
    }
    else barf(C_BAD_ARGUMENT_TYPE_ERROR, "assv", a);
  
    lst = C_u_i_cdr(lst);
  }

  if(lst!=C_SCHEME_END_OF_LIST)
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "assv", lst);

  return C_SCHEME_FALSE;
}


C_regparm C_word C_fcall C_i_assoc(C_word x, C_word lst)
{
  C_word a;

  while(!C_immediatep(lst) && C_block_header(lst) == C_PAIR_TAG) {
    a = C_u_i_car(lst);

    if(!C_immediatep(a) && C_block_header(a) == C_PAIR_TAG) {
      if(C_equalp(C_u_i_car(a), x)) return a;
    }
    else barf(C_BAD_ARGUMENT_TYPE_ERROR, "assoc", a);
  
    lst = C_u_i_cdr(lst);
  }

  if(lst!=C_SCHEME_END_OF_LIST)
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "assoc", lst);

  return C_SCHEME_FALSE;
}


C_regparm C_word C_fcall C_i_memq(C_word x, C_word lst)
{
  while(!C_immediatep(lst) && C_block_header(lst) == C_PAIR_TAG) {
    if(C_u_i_car(lst) == x) return lst;
    else lst = C_u_i_cdr(lst);
  }

  if(lst!=C_SCHEME_END_OF_LIST)
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "memq", lst);

  return C_SCHEME_FALSE;
}


C_regparm C_word C_fcall C_u_i_memq(C_word x, C_word lst)
{
  while(!C_immediatep(lst)) {
    if(C_u_i_car(lst) == x) return lst;
    else lst = C_u_i_cdr(lst);
  }

  return C_SCHEME_FALSE;
}


C_regparm C_word C_fcall C_i_memv(C_word x, C_word lst)
{
  while(!C_immediatep(lst) && C_block_header(lst) == C_PAIR_TAG) {
    if(C_truep(C_i_eqvp(C_u_i_car(lst), x))) return lst;
    else lst = C_u_i_cdr(lst);
  }

  if(lst!=C_SCHEME_END_OF_LIST)
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "memv", lst);
  
  return C_SCHEME_FALSE;
}


C_regparm C_word C_fcall C_i_member(C_word x, C_word lst)
{
  while(!C_immediatep(lst) && C_block_header(lst) == C_PAIR_TAG) {
    if(C_equalp(C_u_i_car(lst), x)) return lst;
    else lst = C_u_i_cdr(lst);
  }

  if(lst!=C_SCHEME_END_OF_LIST)
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "member", lst);
  
  return C_SCHEME_FALSE;
}


/* Inline routines for extended bindings: */

C_regparm C_word C_fcall C_i_check_closure_2(C_word x, C_word loc)
{
  if(C_immediatep(x) || (C_header_bits(x) != C_CLOSURE_TYPE)) {
    error_location = loc;
    barf(C_BAD_ARGUMENT_TYPE_NO_CLOSURE_ERROR, NULL, x);
  }

  return C_SCHEME_UNDEFINED;
}


C_regparm C_word C_fcall C_i_check_exact_2(C_word x, C_word loc)
{
  if((x & C_FIXNUM_BIT) == 0) {
    error_location = loc;
    barf(C_BAD_ARGUMENT_TYPE_NO_FIXNUM_ERROR, NULL, x);
  }

  return C_SCHEME_UNDEFINED;
}


C_regparm C_word C_fcall C_i_check_inexact_2(C_word x, C_word loc)
{
  if(C_immediatep(x) || C_block_header(x) != C_FLONUM_TAG) {
    error_location = loc;
    barf(C_BAD_ARGUMENT_TYPE_NO_FLONUM_ERROR, NULL, x);
  }

  return C_SCHEME_UNDEFINED;
}


C_regparm C_word C_fcall C_i_check_char_2(C_word x, C_word loc)
{
  if((x & C_IMMEDIATE_TYPE_BITS) != C_CHARACTER_BITS) {
    error_location = loc;
    barf(C_BAD_ARGUMENT_TYPE_NO_CHAR_ERROR, NULL, x);
  }

  return C_SCHEME_UNDEFINED;
}


C_regparm C_word C_fcall C_i_check_number_2(C_word x, C_word loc)
{
  if((x & C_FIXNUM_BIT) == 0 && (C_immediatep(x) || C_block_header(x) != C_FLONUM_TAG)) {
    error_location = loc;
    barf(C_BAD_ARGUMENT_TYPE_NO_NUMBER_ERROR, NULL, x);
  }

  return C_SCHEME_UNDEFINED;
}


C_regparm C_word C_fcall C_i_check_string_2(C_word x, C_word loc)
{
  if(C_immediatep(x) || C_header_bits(x) != C_STRING_TYPE) {
    error_location = loc;
    barf(C_BAD_ARGUMENT_TYPE_NO_STRING_ERROR, NULL, x);
  }

  return C_SCHEME_UNDEFINED;
}


C_regparm C_word C_fcall C_i_check_bytevector_2(C_word x, C_word loc)
{
  if(C_immediatep(x) || C_header_bits(x) != C_BYTEVECTOR_TYPE) {
    error_location = loc;
    barf(C_BAD_ARGUMENT_TYPE_NO_BYTEVECTOR_ERROR, NULL, x);
  }

  return C_SCHEME_UNDEFINED;
}


C_regparm C_word C_fcall C_i_check_vector_2(C_word x, C_word loc)
{
  if(C_immediatep(x) || C_header_bits(x) != C_VECTOR_TYPE) {
    error_location = loc;
    barf(C_BAD_ARGUMENT_TYPE_NO_VECTOR_ERROR, NULL, x);
  }

  return C_SCHEME_UNDEFINED;
}


C_regparm C_word C_fcall C_i_check_structure_2(C_word x, C_word st, C_word loc)
{
  if(C_immediatep(x) || C_header_bits(x) != C_STRUCTURE_TYPE || C_block_item(x,0) != st) {
    error_location = loc;
    barf(C_BAD_ARGUMENT_TYPE_BAD_STRUCT_ERROR, NULL, x, st);
  }

  return C_SCHEME_UNDEFINED;
}


C_regparm C_word C_fcall C_i_check_pair_2(C_word x, C_word loc)
{
  if(C_immediatep(x) || C_block_header(x) != C_PAIR_TAG) {
    error_location = loc;
    barf(C_BAD_ARGUMENT_TYPE_NO_PAIR_ERROR, NULL, x);
  }

  return C_SCHEME_UNDEFINED;
}


C_regparm C_word C_fcall C_i_check_boolean_2(C_word x, C_word loc)
{
  if((x & C_IMMEDIATE_TYPE_BITS) != C_BOOLEAN_BITS) {
    error_location = loc;
    barf(C_BAD_ARGUMENT_TYPE_NO_BOOLEAN_ERROR, NULL, x);
  }

  return C_SCHEME_UNDEFINED;
}


C_regparm C_word C_fcall C_i_check_locative_2(C_word x, C_word loc)
{
  if(C_immediatep(x) || C_block_header(x) != C_LOCATIVE_TAG) {
    error_location = loc;
    barf(C_BAD_ARGUMENT_TYPE_NO_LOCATIVE_ERROR, NULL, x);
  }

  return C_SCHEME_UNDEFINED;
}


C_regparm C_word C_fcall C_i_check_symbol_2(C_word x, C_word loc)
{
  if(C_immediatep(x) || C_block_header(x) != C_SYMBOL_TAG) {
    error_location = loc;
    barf(C_BAD_ARGUMENT_TYPE_NO_SYMBOL_ERROR, NULL, x);
  }

  return C_SCHEME_UNDEFINED;
}


C_regparm C_word C_fcall C_i_check_list_2(C_word x, C_word loc)
{
  if(x != C_SCHEME_END_OF_LIST && (C_immediatep(x) || C_block_header(x) != C_PAIR_TAG)) {
    error_location = loc;
    barf(C_BAD_ARGUMENT_TYPE_NO_LIST_ERROR, NULL, x);
  }

  return C_SCHEME_UNDEFINED;
}


C_regparm C_word C_fcall C_i_check_port_2(C_word x, C_word input, C_word open, C_word loc)
{
  int inp;

  if(C_immediatep(x) || C_header_bits(x) != C_PORT_TYPE) {
    error_location = loc;
    barf(C_BAD_ARGUMENT_TYPE_NO_PORT_ERROR, NULL, x);
  }

  inp = C_block_item(x, 1) == C_SCHEME_TRUE;	/* slot #1: I/O flag */

  switch(input) {
  case C_SCHEME_TRUE:
    if(!inp) {
      error_location = loc;
      barf(C_BAD_ARGUMENT_TYPE_NO_INPUT_PORT_ERROR, NULL, x);
    }

    break;

  case C_SCHEME_FALSE:
    if(inp) {
      error_location = loc;
      barf(C_BAD_ARGUMENT_TYPE_NO_OUTPUT_PORT_ERROR, NULL, x);
    }

    break;

    /* any other value: omit direction check */
  }

  if(open == C_SCHEME_TRUE) {
    if(C_block_item(x, 8) != C_SCHEME_FALSE) {	/* slot #8: closed flag */
      error_location = loc;
      barf(C_PORT_CLOSED_ERROR, NULL, x);
    }
  }

  return C_SCHEME_UNDEFINED;
}


/*XXX these are not correctly named */
C_regparm C_word C_fcall C_i_foreign_char_argumentp(C_word x)
{
  if((x & C_IMMEDIATE_TYPE_BITS) != C_CHARACTER_BITS)
    barf(C_BAD_ARGUMENT_TYPE_NO_CHAR_ERROR, NULL, x);

  return x;
}


C_regparm C_word C_fcall C_i_foreign_fixnum_argumentp(C_word x)
{
  if((x & C_FIXNUM_BIT) == 0)
    barf(C_BAD_ARGUMENT_TYPE_NO_FIXNUM_ERROR, NULL, x);

  return x;
}


C_regparm C_word C_fcall C_i_foreign_flonum_argumentp(C_word x)
{
  if((x & C_FIXNUM_BIT) != 0) return x;

  if(C_immediatep(x) || C_block_header(x) != C_FLONUM_TAG)
    barf(C_BAD_ARGUMENT_TYPE_NO_FLONUM_ERROR, NULL, x);

  return x;
}


C_regparm C_word C_fcall C_i_foreign_block_argumentp(C_word x)
{
  if(C_immediatep(x))
    barf(C_BAD_ARGUMENT_TYPE_NO_BLOCK_ERROR, NULL, x);

  return x;
}


C_regparm C_word C_fcall C_i_foreign_struct_wrapper_argumentp(C_word t, C_word x)
{
  if(C_immediatep(x) || C_header_bits(x) != C_STRUCTURE_TYPE || C_block_item(x, 0) != t)
    barf(C_BAD_ARGUMENT_TYPE_BAD_STRUCT_ERROR, NULL, t, x);

  return x;
}


C_regparm C_word C_fcall C_i_foreign_string_argumentp(C_word x)
{
  if(C_immediatep(x) || C_header_bits(x) != C_STRING_TYPE)
    barf(C_BAD_ARGUMENT_TYPE_NO_STRING_ERROR, NULL, x);

  return x;
}


C_regparm C_word C_fcall C_i_foreign_symbol_argumentp(C_word x)
{
  if(C_immediatep(x) || C_header_bits(x) != C_SYMBOL_TYPE)
    barf(C_BAD_ARGUMENT_TYPE_NO_SYMBOL_ERROR, NULL, x);

  return x;
}


C_regparm C_word C_fcall C_i_foreign_pointer_argumentp(C_word x)
{
  if(C_immediatep(x) || 
     (C_header_bits(x) != C_SWIG_POINTER_TYPE &&
      (C_header_bits(x) & C_SPECIALBLOCK_BIT) == 0) )
    barf(C_BAD_ARGUMENT_TYPE_NO_POINTER_ERROR, NULL, x);

  return x;
}


C_regparm C_word C_fcall C_i_foreign_scheme_or_c_pointer_argumentp(C_word x)
{
  if(C_immediatep(x) || 
     (C_header_bits(x) != C_SWIG_POINTER_TYPE &&
      (C_header_bits(x) & C_SPECIALBLOCK_BIT) == 0) )
    barf(C_BAD_ARGUMENT_TYPE_NO_POINTER_ERROR, NULL, x);

  return x;
}


C_regparm C_word C_fcall C_i_foreign_tagged_pointer_argumentp(C_word x, C_word t)
{
  if(C_immediatep(x) || (C_header_bits(x) & C_SPECIALBLOCK_BIT) == 0
     || (t != C_SCHEME_FALSE && !C_equalp(C_block_item(x, 1), t)))
    barf(C_BAD_ARGUMENT_TYPE_NO_TAGGED_POINTER_ERROR, NULL, x, t);

  return x;
}


C_regparm C_word C_fcall C_i_foreign_integer_argumentp(C_word x)
{
  double m;

  if((x & C_FIXNUM_BIT) != 0) return x;

  if(!C_immediatep(x) && C_block_header(x) == C_FLONUM_TAG) {
    m = C_flonum_magnitude(x);

    if(m >= C_WORD_MIN && m <= C_WORD_MAX) return x;
  }

  barf(C_BAD_ARGUMENT_TYPE_NO_INTEGER_ERROR, NULL, x);
  return C_SCHEME_UNDEFINED;
}


C_regparm C_word C_fcall C_i_foreign_integer64_argumentp(C_word x)
{
  double m, r;

  if((x & C_FIXNUM_BIT) != 0) return x;
  
  if(!C_immediatep(x) && C_block_header(x) == C_FLONUM_TAG) {
    m = C_flonum_magnitude(x);

    if(m >= C_S64_MIN && m <= C_S64_MAX && C_modf(m, &r) == 0.0) return x;
  }

  barf(C_BAD_ARGUMENT_TYPE_NO_INTEGER_ERROR, NULL, x);
  return C_SCHEME_UNDEFINED;
}


C_regparm C_word C_fcall C_i_foreign_unsigned_integer_argumentp(C_word x)
{
  double m ,r;

  if((x & C_FIXNUM_BIT) != 0) return x;

  if(!C_immediatep(x) && C_block_header(x) == C_FLONUM_TAG) {
    m = C_flonum_magnitude(x);

    if(m >= 0 && m <= C_UWORD_MAX && C_modf(m, &r) == 0.0) return x;
  }

  barf(C_BAD_ARGUMENT_TYPE_NO_UINTEGER_ERROR, NULL, x);
  return C_SCHEME_UNDEFINED;
}


C_regparm C_word C_fcall C_i_foreign_unsigned_integer64_argumentp(C_word x)
{
  double m, r;

  if((x & C_FIXNUM_BIT) != 0) return x;

  if(!C_immediatep(x) && C_block_header(x) == C_FLONUM_TAG) {
    m = C_flonum_magnitude(x);

    if(m >= 0 && m <= C_U64_MAX && C_modf(m, &r) == 0.0) return x;
  }

  barf(C_BAD_ARGUMENT_TYPE_NO_UINTEGER_ERROR, NULL, x);
  return C_SCHEME_UNDEFINED;
}


/* I */
C_regparm C_word C_fcall C_i_not_pair_p_2(C_word x)
{
  return C_mk_bool(C_immediatep(x) || C_block_header(x) != C_PAIR_TAG);
}


C_regparm C_word C_fcall C_i_null_list_p(C_word x)
{
  if(x == C_SCHEME_END_OF_LIST) return C_SCHEME_TRUE;
  else if(!C_immediatep(x) && C_block_header(x) == C_PAIR_TAG) return C_SCHEME_FALSE;
  else {
    barf(C_BAD_ARGUMENT_TYPE_NO_LIST_ERROR, "null-list?", x);
    return C_SCHEME_FALSE;
  }
}


C_regparm C_word C_fcall C_i_string_null_p(C_word x)
{
  if(!C_immediatep(x) && C_header_bits(x) == C_STRING_TYPE)
    return C_zero_length_p(x);
  else {
    barf(C_BAD_ARGUMENT_TYPE_NO_STRING_ERROR, "string-null?", x);
    return C_SCHEME_FALSE;
  }
}


C_regparm C_word C_fcall C_i_null_pointerp(C_word x)
{
  if(!C_immediatep(x) && (C_header_bits(x) & C_SPECIALBLOCK_BIT) != 0)
    return C_null_pointerp(x);

  barf(C_BAD_ARGUMENT_TYPE_ERROR, "null-pointer?", x);
  return C_SCHEME_FALSE;
}

C_regparm C_word C_i_char_equalp(C_word x, C_word y)
{
  C_i_check_char_2(x, intern0("char=?"));
  C_i_check_char_2(y, intern0("char=?"));
  return C_u_i_char_equalp(x, y);
}

C_regparm C_word C_i_char_greaterp(C_word x, C_word y)
{
  C_i_check_char_2(x, intern0("char>?"));
  C_i_check_char_2(y, intern0("char>?"));
  return C_u_i_char_greaterp(x, y);
}

C_regparm C_word C_i_char_lessp(C_word x, C_word y)
{
  C_i_check_char_2(x, intern0("char<?"));
  C_i_check_char_2(y, intern0("char<?"));
  return C_u_i_char_lessp(x, y);
}

C_regparm C_word C_i_char_greater_or_equal_p(C_word x, C_word y)
{
  C_i_check_char_2(x, intern0("char>=?"));
  C_i_check_char_2(y, intern0("char>=?"));
  return C_u_i_char_greater_or_equal_p(x, y);
}

C_regparm C_word C_i_char_less_or_equal_p(C_word x, C_word y)
{
  C_i_check_char_2(x, intern0("char<=?"));
  C_i_check_char_2(y, intern0("char<=?"));
  return C_u_i_char_less_or_equal_p(x, y);
}


/* Primitives: */

void C_ccall C_apply(C_word c, C_word *av)
{
  C_word
    /* closure = av[ 0 ] */
    k = av[ 1 ],
    fn = av[ 2 ];
  int av2_size, i, n = c - 3;
  int non_list_args = n - 1;
  C_word lst, len, *ptr, *av2;

  if(c < 4) C_bad_min_argc(c, 4);

  if(C_immediatep(fn) || C_header_bits(fn) != C_CLOSURE_TYPE)
    barf(C_NOT_A_CLOSURE_ERROR, "apply", fn);

  lst = av[ c - 1 ];
  if(lst != C_SCHEME_END_OF_LIST && (C_immediatep(lst) || C_block_header(lst) != C_PAIR_TAG))
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "apply", lst);

  len = C_unfix(C_u_i_length(lst));
  av2_size = 2 + non_list_args + len;

  if(C_demand(av2_size))
    stack_check_demand = 0;
  else if(stack_check_demand)
    C_stack_overflow_with_loc("apply");
  else {
    stack_check_demand = av2_size;
    C_save_and_reclaim((void *)C_apply, c, av);
  }

  av2 = ptr = C_alloc(av2_size);
  *(ptr++) = fn;
  *(ptr++) = k;

  if(non_list_args > 0) {
    C_memcpy(ptr, av + 3, non_list_args * sizeof(C_word));
    ptr += non_list_args;
  }

  while(len--) {
    *(ptr++) = C_u_i_car(lst);
    lst = C_u_i_cdr(lst);
  }

  assert((ptr - av2) == av2_size);

  ((C_proc)(void *)C_block_item(fn, 0))(av2_size, av2);
}


void C_ccall C_call_cc(C_word c, C_word *av)
{
  C_word
    /* closure = av[ 0 ] */
    k = av[ 1 ],
    cont = av[ 2 ],
    *a = C_alloc(3),
    wrapper;
  void *pr = (void *)C_block_item(cont,0);
  C_word av2[ 3 ];
  
  if(C_immediatep(cont) || C_header_bits(cont) != C_CLOSURE_TYPE)
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "call-with-current-continuation", cont);
  
  /* Check for values-continuation: */
  if(C_block_item(k, 0) == (C_word)values_continuation)
    wrapper = C_closure(&a, 2, (C_word)call_cc_values_wrapper, k);
  else wrapper = C_closure(&a, 2, (C_word)call_cc_wrapper, k);
  
  av2[ 0 ] = cont;
  av2[ 1 ] = k;
  av2[ 2 ] = wrapper;
  ((C_proc)pr)(3, av2);
}


void C_ccall call_cc_wrapper(C_word c, C_word *av)
{
  C_word
    closure = av[ 0 ],
    /* av[ 1 ] is current k and ignored */
    result,
    k = C_block_item(closure, 1);

  if(c != 3) C_bad_argc(c, 3);

  result = av[ 2 ];
  C_kontinue(k, result);
}


void C_ccall call_cc_values_wrapper(C_word c, C_word *av)
{
  C_word
    closure = av[ 0 ],
    /* av[ 1 ] is current k and ignored */
    k = C_block_item(closure, 1),
    x1,
    n = c;
  
  av[ 0 ] = k;               /* reuse av */
  C_memmove(av + 1, av + 2, (n - 1) * sizeof(C_word));
  C_do_apply(n - 1, av);
}


/* I */
void C_ccall C_continuation_graft(C_word c, C_word *av)
{
  C_word
    /* self = av[ 0 ] */
    /* k = av[ 1 ] */
    kk = av[ 2 ],
    proc = av[ 3 ];

  av[ 0 ] = proc;               /* reuse av */
  av[ 1 ] = C_block_item(kk, 1);
  ((C_proc)C_fast_retrieve_proc(proc))(2, av);
}


void C_ccall C_values(C_word c, C_word *av)
{
  C_word
    /* closure = av[ 0 ] */
    k = av[ 1 ],
    n = c;

  if(c < 2) C_bad_min_argc(c, 2);

  /* Check continuation whether it receives multiple values: */
  if(C_block_item(k, 0) == (C_word)values_continuation) {
    av[ 0 ] = k;                /* reuse av */
    C_memmove(av + 1, av + 2, (c - 2) * sizeof(C_word));
    C_do_apply(c - 1, av);
  }
  
  if(c != 3) {
#ifdef RELAX_MULTIVAL_CHECK
    if(c == 2) n = C_SCHEME_UNDEFINED;
    else n = av[ 2 ];
#else
    barf(C_CONTINUATION_CANT_RECEIVE_VALUES_ERROR, "values", k);
#endif
  }
  else n = av[ 2 ];

  C_kontinue(k, n);
}


void C_ccall C_apply_values(C_word c, C_word *av)
{
  C_word
    /* closure = av[ 0 ] */
    k = av[ 1 ],
    lst, len, n;

  if(c != 3) C_bad_argc(c, 3);

  lst = av[ 2 ];

  if(lst != C_SCHEME_END_OF_LIST && (C_immediatep(lst) || C_block_header(lst) != C_PAIR_TAG))
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "apply", lst);

  /* Check whether continuation receives multiple values: */
  if(C_block_item(k, 0) == (C_word)values_continuation) {
    C_word *av2, *ptr;

    len = C_unfix(C_u_i_length(lst));
    n = len + 1;

    if(C_demand(n))
      stack_check_demand = 0;
    else if(stack_check_demand)
      C_stack_overflow_with_loc("apply");
    else {
      stack_check_demand = n;
      C_save_and_reclaim((void *)C_apply_values, c, av);
    }

    av2 = C_alloc(n);
    av2[ 0 ] = k;
    ptr = av2 + 1;
    while(len--) {
      *(ptr++) = C_u_i_car(lst);
      lst = C_u_i_cdr(lst);
    }

    C_do_apply(n, av2);
  }
  
  if(C_immediatep(lst)) {
#ifdef RELAX_MULTIVAL_CHECK
    n = C_SCHEME_UNDEFINED;
#else
    barf(C_CONTINUATION_CANT_RECEIVE_VALUES_ERROR, "values", k);
#endif
  }
  else if(C_block_header(lst) == C_PAIR_TAG) {
    if(C_u_i_cdr(lst) == C_SCHEME_END_OF_LIST)
      n = C_u_i_car(lst);
    else {
#ifdef RELAX_MULTIVAL_CHECK
      n = C_u_i_car(lst);
#else
      barf(C_CONTINUATION_CANT_RECEIVE_VALUES_ERROR, "values", k);
#endif
    }
  }
  else barf(C_BAD_ARGUMENT_TYPE_ERROR, "apply", lst);
  
  C_kontinue(k, n);
}


void C_ccall C_call_with_values(C_word c, C_word *av)
{
  C_word
    /* closure = av[ 0 ] */
    k = av[ 1 ],
    thunk,
    kont,
    *a = C_alloc(4),
    kk;

  if(c != 4) C_bad_argc(c, 4);

  thunk = av[ 2 ];
  kont = av[ 3 ];

  if(C_immediatep(thunk) || C_header_bits(thunk) != C_CLOSURE_TYPE)
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "call-with-values", thunk);

  if(C_immediatep(kont) || C_header_bits(kont) != C_CLOSURE_TYPE)
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "call-with-values", kont);

  kk = C_closure(&a, 3, (C_word)values_continuation, kont, k);
  av[ 0 ] = thunk;              /* reuse av */
  av[ 1 ] = kk;
  C_do_apply(2, av);
}


void C_ccall C_u_call_with_values(C_word c, C_word *av)
{
  C_word
    /* closure = av[ 0 ] */
    k = av[ 1 ],
    thunk = av[ 2 ],
    kont = av[ 3 ],
    *a = C_alloc(4),
    kk;

  kk = C_closure(&a, 3, (C_word)values_continuation, kont, k);
  av[ 0 ] = thunk;              /* reuse av */
  av[ 1 ] = kk;
  C_do_apply(2, av);
}


void C_ccall values_continuation(C_word c, C_word *av)
{
  C_word
    closure = av[ 0 ],
    kont = C_block_item(closure, 1),
    k = C_block_item(closure, 2),
    *av2 = C_alloc(c + 1);

  av2[ 0 ] = kont;
  av2[ 1 ] = k;
  C_memcpy(av2 + 2, av + 1, (c - 1) * sizeof(C_word));
  C_do_apply(c + 1, av2);
}


void C_ccall C_times(C_word c, C_word *av)
{
  C_word
    /* closure = av[ 0 ] */
    k = av[ 1 ],
    x, y,
    iresult = C_fix(1);
  double fresult;
  C_alloc_flonum;

  c -= 2;
  av += 2;

  while(c--) {
    x = *(av++);
    
    if(x & C_FIXNUM_BIT) {
      y = C_i_o_fixnum_times(iresult, x);

      if(y == C_SCHEME_FALSE) {
	fresult = (double)C_unfix(iresult) * (double)C_unfix(x);
	goto flonum_result;
      }
      else iresult = y;
    }
    else if(!C_immediatep(x) && C_block_header(x) == C_FLONUM_TAG) {
      fresult = (double)C_unfix(iresult) * C_flonum_magnitude(x);
      goto flonum_result;
    }
    else barf(C_BAD_ARGUMENT_TYPE_ERROR, "*", x);
  }

  C_kontinue(k, iresult);

 flonum_result:
  while(c--) {
    x = *(av++);

    if(x & C_FIXNUM_BIT)
      fresult *= (double)C_unfix(x);
    else if(!C_immediatep(x) && C_block_header(x) == C_FLONUM_TAG)
      fresult *= C_flonum_magnitude(x);
    else barf(C_BAD_ARGUMENT_TYPE_ERROR, "*", x);
  }

  C_kontinue_flonum(k, fresult);
}


C_regparm C_word C_fcall C_2_times(C_word **ptr, C_word x, C_word y)
{
  C_word iresult;

  if(x & C_FIXNUM_BIT) {
    if(y & C_FIXNUM_BIT) {
      iresult = C_i_o_fixnum_times(x, y);

      if(iresult == C_SCHEME_FALSE)
	return C_flonum(ptr, (double)C_unfix(x) * (double)C_unfix(y));
      else return iresult;
    }
    else if(!C_immediatep(y) && C_block_header(y) == C_FLONUM_TAG)
      return C_flonum(ptr, (double)C_unfix(x) * C_flonum_magnitude(y));
    else barf(C_BAD_ARGUMENT_TYPE_ERROR, "*", y);
  }
  else if(!C_immediatep(x) && C_block_header(x) == C_FLONUM_TAG) {
    if(y & C_FIXNUM_BIT) 
      return C_flonum(ptr, C_flonum_magnitude(x) * (double)C_unfix(y));
    else if(!C_immediatep(y) && C_block_header(y) == C_FLONUM_TAG)
      return C_flonum(ptr, C_flonum_magnitude(x) * C_flonum_magnitude(y));
    else barf(C_BAD_ARGUMENT_TYPE_ERROR, "*", y);
  }
  else barf(C_BAD_ARGUMENT_TYPE_ERROR, "*", x);
  /* shutup compiler */
  return C_flonum(ptr, 0.0/0.0);
}


void C_ccall C_plus(C_word c, C_word *av)
{
  C_word
    /* closure = av[ 0 ] */
    k = av[ 1 ],
    x, y,
    iresult = C_fix(0);
  double fresult;
  C_alloc_flonum;

  c -= 2;
  av += 2;

  while(c--) {
    x = *(av++);
    
    if(x & C_FIXNUM_BIT) {
      y = C_i_o_fixnum_plus(iresult, x);

      if(y == C_SCHEME_FALSE) {
	fresult = (double)C_unfix(iresult) + (double)C_unfix(x);
	goto flonum_result;
      }
      else iresult = y;
    }
    else if(!C_immediatep(x) && C_block_header(x) == C_FLONUM_TAG) {
      fresult = (double)C_unfix(iresult) + C_flonum_magnitude(x);
      goto flonum_result;
    }
    else barf(C_BAD_ARGUMENT_TYPE_ERROR, "+", x);
  }

  C_kontinue(k, iresult);

 flonum_result:
  while(c--) {
    x = *(av++);

    if(x & C_FIXNUM_BIT)
      fresult += (double)C_unfix(x);
    else if(!C_immediatep(x) && C_block_header(x) == C_FLONUM_TAG)
      fresult += C_flonum_magnitude(x);
    else barf(C_BAD_ARGUMENT_TYPE_ERROR, "+", x);
  }

  C_kontinue_flonum(k, fresult);
}


C_regparm C_word C_fcall C_2_plus(C_word **ptr, C_word x, C_word y)
{
  C_word iresult;

  if(x & C_FIXNUM_BIT) {
    if(y & C_FIXNUM_BIT) {
      iresult = C_i_o_fixnum_plus(x, y);

      if(iresult == C_SCHEME_FALSE)
	return C_flonum(ptr, (double)C_unfix(x) + (double)C_unfix(y));
      else return iresult;
    }
    else if(!C_immediatep(y) && C_block_header(y) == C_FLONUM_TAG)
      return C_flonum(ptr, (double)C_unfix(x) + C_flonum_magnitude(y));
    else barf(C_BAD_ARGUMENT_TYPE_ERROR, "+", y);
  }
  else if(!C_immediatep(x) && C_block_header(x) == C_FLONUM_TAG) {
    if(y & C_FIXNUM_BIT) 
      return C_flonum(ptr, C_flonum_magnitude(x) + (double)C_unfix(y));
    else if(!C_immediatep(y) && C_block_header(y) == C_FLONUM_TAG)
      return C_flonum(ptr, C_flonum_magnitude(x) + C_flonum_magnitude(y));
    else barf(C_BAD_ARGUMENT_TYPE_ERROR, "+", y);
  }
  else barf(C_BAD_ARGUMENT_TYPE_ERROR, "+", x);
  /* shutup compiler */
  return C_flonum(ptr, 0.0/0.0);
}


void C_ccall C_minus(C_word c, C_word *av)
{
  C_word
    /* closure = av[ 0 ] */
    k = av[ 1 ],
    x, y, n1,
    iresult;
  double fresult;
  int ff = 0;
  C_alloc_flonum;

  if(c < 3) C_bad_min_argc(c, 3);

  n1 = av[ 2 ];

  if(n1 & C_FIXNUM_BIT) iresult = n1;
  else if(!C_immediatep(n1) && C_block_header(n1) == C_FLONUM_TAG) {
    fresult = C_flonum_magnitude(n1);
    ff = 1;
  }
  else barf(C_BAD_ARGUMENT_TYPE_ERROR, "-", n1);

  if(c == 3) {
    if(!ff) {
      C_kontinue(k, C_fix(-C_unfix(n1)));
    }
    else {
      C_kontinue_flonum(k, -fresult);
    }
  }

  c -= 3;
  av += 3;

  if(ff) goto flonum_result;

  while(c--) {
    x = *(av++);
    
    if(x & C_FIXNUM_BIT) {
      y = C_i_o_fixnum_difference(iresult, x);

      if(y == C_SCHEME_FALSE) {
	fresult = (double)C_unfix(iresult) - (double)C_unfix(x);
	goto flonum_result;
      }
      else iresult = y;
    }
    else if(!C_immediatep(x) && C_block_header(x) == C_FLONUM_TAG) {
      fresult = (double)C_unfix(iresult) - C_flonum_magnitude(x);
      goto flonum_result;
    }
    else barf(C_BAD_ARGUMENT_TYPE_ERROR, "-", x);
  }

  C_kontinue(k, iresult);

 flonum_result:
  while(c--) {
    x = *(av++);

    if(x & C_FIXNUM_BIT)
      fresult -= (double)C_unfix(x);
    else if(!C_immediatep(x) && C_block_header(x) == C_FLONUM_TAG)
      fresult -= C_flonum_magnitude(x);
    else barf(C_BAD_ARGUMENT_TYPE_ERROR, "-", x);
  }

  C_kontinue_flonum(k, fresult);
}


C_regparm C_word C_fcall C_2_minus(C_word **ptr, C_word x, C_word y)
{
  C_word iresult;

  if(x & C_FIXNUM_BIT) {
    if(y & C_FIXNUM_BIT) {
      iresult = C_i_o_fixnum_difference(x, y);

      if(iresult == C_SCHEME_FALSE)
	return C_flonum(ptr, (double)C_unfix(x) - (double)C_unfix(y));
      else return iresult;
    }
    else if(!C_immediatep(y) && C_block_header(y) == C_FLONUM_TAG)
      return C_flonum(ptr, (double)C_unfix(x) - C_flonum_magnitude(y));
    else barf(C_BAD_ARGUMENT_TYPE_ERROR, "-", y);
  }
  else if(!C_immediatep(x) && C_block_header(x) == C_FLONUM_TAG) {
    if(y & C_FIXNUM_BIT) 
      return C_flonum(ptr, C_flonum_magnitude(x) - (double)C_unfix(y));
    else if(!C_immediatep(y) && C_block_header(y) == C_FLONUM_TAG)
      return C_flonum(ptr, C_flonum_magnitude(x) - C_flonum_magnitude(y));
    else barf(C_BAD_ARGUMENT_TYPE_ERROR, "-", y);
  }
  else barf(C_BAD_ARGUMENT_TYPE_ERROR, "-", x);
  /* shutup compiler */
  return C_flonum(ptr, 0.0/0.0);
}



void C_ccall C_divide(C_word c, C_word *av)
{
  C_word
    /* closure = av[ 0 ] */
    k = av[ 1 ],
    n1, n2,
    iresult, n3;
  int fflag;
  double fresult, f2;
  C_alloc_flonum;

  if(c < 3) C_bad_min_argc(c, 3);

  n1 = av[ 2 ];

  if(n1 & C_FIXNUM_BIT) {
    iresult = C_unfix(n1);
    fflag = 0;
  }
  else if(!C_immediatep(n1) && C_block_header(n1) == C_FLONUM_TAG) {
    fresult = C_flonum_magnitude(n1);
    fflag = 1;
  }
  else barf(C_BAD_ARGUMENT_TYPE_ERROR, "/", n1);

  if(c == 3) {
    if(fflag) {
      if(fresult == 0) barf(C_DIVISION_BY_ZERO_ERROR, "/");

      fresult = 1.0 / fresult;
    }
    else {
      if(iresult == 0) barf(C_DIVISION_BY_ZERO_ERROR, "/");
      else if(iresult == 1) C_kontinue(k, C_fix(1));

      fresult = 1.0 / (double)iresult;
      fflag = 1;
    }

    goto cont;
  }

  c -= 3;
  av += 3;

  while(c--) {
    n1 = *(av++);
    
    if(n1 & C_FIXNUM_BIT) {
      if(fflag) {
	if((n1 = C_unfix(n1)) == 0) 
	  barf(C_DIVISION_BY_ZERO_ERROR, "/");

	fresult /= n1;
      }
      else {
	if((n2 = C_unfix(n1)) == 0)
	  barf(C_DIVISION_BY_ZERO_ERROR, "/");

	n3 = iresult / n2;

	if((fresult = (double)iresult / (double)n2) != n3)
	  fflag = 1;
	else iresult = n3;
      }
    }
    else if(!C_immediatep(n1) && C_block_header(n1) == C_FLONUM_TAG) {
      if(fflag) {
	if((f2 = C_flonum_magnitude(n1)) == 0)
	  barf(C_DIVISION_BY_ZERO_ERROR, "/");

	fresult /= f2;
      }
      else {
	fflag = 1;

	if((f2 = C_flonum_magnitude(n1)) == 0)
	  barf(C_DIVISION_BY_ZERO_ERROR, "/");

	fresult = (double)iresult / f2;
      }
    }
    else barf(C_BAD_ARGUMENT_TYPE_ERROR, "/", n1);
  }

 cont:
  if(fflag) {
    C_kontinue_flonum(k, fresult);
  }
  else n1 = C_fix(iresult);

  C_kontinue(k, n1);
}


C_regparm C_word C_fcall C_2_divide(C_word **ptr, C_word x, C_word y)
{
  C_word iresult;
  double fresult;
  int fflag = 0;

  if(x & C_FIXNUM_BIT) {
    if(y & C_FIXNUM_BIT) {
      if((iresult = C_unfix(y)) == 0) barf(C_DIVISION_BY_ZERO_ERROR, "/");

      fresult = (double)C_unfix(x) / (double)iresult;
      iresult = C_unfix(x) / iresult;
    }
    else if(!C_immediatep(y) && C_block_header(y) == C_FLONUM_TAG) {
      if((fresult = C_flonum_magnitude(y)) == 0.0)
	barf(C_DIVISION_BY_ZERO_ERROR, "/");

      fresult = (double)C_unfix(x) / fresult;
      fflag = 1;
    }
    else barf(C_BAD_ARGUMENT_TYPE_ERROR, "/", y);
  }
  else if(!C_immediatep(x) && C_block_header(x) == C_FLONUM_TAG) {
    fflag = 1;

    if(y & C_FIXNUM_BIT) {
      fresult = C_flonum_magnitude(x);

      if((iresult = C_unfix(y)) == 0) barf(C_DIVISION_BY_ZERO_ERROR, "/");

      fresult = fresult / (double)iresult;
    }
    else if(!C_immediatep(y) && C_block_header(y) == C_FLONUM_TAG) {
      if((fresult = C_flonum_magnitude(y)) == 0.0) barf(C_DIVISION_BY_ZERO_ERROR, "/");
      
      fresult = C_flonum_magnitude(x) / fresult;
    }
    else barf(C_BAD_ARGUMENT_TYPE_ERROR, "/", y);
  }
  else barf(C_BAD_ARGUMENT_TYPE_ERROR, "/", x);

  if(fflag || (double)iresult != fresult) return C_flonum(ptr, fresult);
  else return C_fix(iresult);
}


void C_ccall C_nequalp(C_word c, C_word *av)
{
  C_word
    /* closure = av[ 0 ] */
    k = av[ 1 ],
    x, i2, f, fflag, ilast;
  double flast, f2;

  c -= 2; 
  f = 1;
  av += 2;

  if(c == 0) goto cont;
  
  x = *(av++);

  if(x & C_FIXNUM_BIT) {
    fflag = 0;
    ilast = C_unfix(x);
  }
  else if(!C_immediatep(x) && C_block_header(x) == C_FLONUM_TAG) {
    fflag = 1;
    flast = C_flonum_magnitude(x);
  }
  else barf(C_BAD_ARGUMENT_TYPE_ERROR, "=", x);

  while(--c) {
    x = *(av++);
    
    if(x & C_FIXNUM_BIT) {
      if(fflag) {
	f = flast == (f2 = (double)C_unfix(x));
        flast = f2;
      }
      else {
	f = ilast == (i2 = C_unfix(x));
	ilast = i2;
      }
    }
    else if(!C_immediatep(x) && C_block_header(x) == C_FLONUM_TAG) {
      if(fflag) {
	f = flast == (f2 = C_flonum_magnitude(x));
	flast = f2;
      }
      else {
	f = (double)ilast == (f2 = C_flonum_magnitude(x));
	flast = f2;
	fflag = 1;
      }
    }
    else barf(C_BAD_ARGUMENT_TYPE_ERROR, "=", x);

    if(!f) break;
  }

 cont:
  C_kontinue(k, C_mk_bool(f));
}


C_regparm C_word C_fcall C_i_nequalp(C_word x, C_word y)
{
  if(x & C_FIXNUM_BIT) {
    if(y & C_FIXNUM_BIT) return C_mk_bool(x == y);
    else if(!C_immediatep(y) && C_block_header(y) == C_FLONUM_TAG)
      return C_mk_bool((double)C_unfix(x) == C_flonum_magnitude(y));

    barf(C_BAD_ARGUMENT_TYPE_NO_NUMBER_ERROR, "=", y);
  }
  else if(!C_immediatep(x) && C_block_header(x) == C_FLONUM_TAG) {
    if(y & C_FIXNUM_BIT) return C_mk_bool(C_flonum_magnitude(x) == (double)C_unfix(y));
    else if(!C_immediatep(y) && C_block_header(y) == C_FLONUM_TAG)
      return C_mk_bool(C_flonum_magnitude(x) == C_flonum_magnitude(y));
    
    barf(C_BAD_ARGUMENT_TYPE_NO_NUMBER_ERROR, "=", y);
  }
  else barf(C_BAD_ARGUMENT_TYPE_NO_NUMBER_ERROR, "=", x);

  return C_SCHEME_FALSE;
}


void C_ccall C_greaterp(C_word c, C_word *av)
{
  C_word 
    /* closure = av[ 0 ] */
    k = av[ 1 ],
    x, i2, f, fflag, ilast;
  double flast, f2;

  c -= 2; 
  f = 1;
  av += 2;

  if(c == 0) goto cont;
  
  x = *(av++);

  if(x & C_FIXNUM_BIT) {
    fflag = 0;
    ilast = C_unfix(x);
  }
  else if(!C_immediatep(x) && C_block_header(x) == C_FLONUM_TAG) {
    fflag = 1;
    flast = C_flonum_magnitude(x);
  }
  else barf(C_BAD_ARGUMENT_TYPE_ERROR, ">", x);

  while(--c) {
    x = *(av++);
    
    if(x & C_FIXNUM_BIT) {
      if(fflag) {
	f = flast > (f2 = (double)C_unfix(x));
        flast = f2;
      }
      else {
	f = ilast > (i2 = C_unfix(x));
	ilast = i2;
      }
    }
    else if(!C_immediatep(x) && C_block_header(x) == C_FLONUM_TAG) {
      if(fflag) {
	f = flast > (f2 = C_flonum_magnitude(x));
	flast = f2;
      }
      else {
	f = (double)ilast > (f2 = C_flonum_magnitude(x));
	flast = f2;
	fflag = 1;
      }
    }
    else barf(C_BAD_ARGUMENT_TYPE_ERROR, ">", x);

    if(!f) break;
  }

 cont:
  C_kontinue(k, C_mk_bool(f));
}


C_regparm C_word C_fcall C_i_greaterp(C_word x, C_word y)
{
  if(x & C_FIXNUM_BIT) {
    if(y & C_FIXNUM_BIT) return C_mk_bool(x > y);
    else if(!C_immediatep(y) && C_block_header(y) == C_FLONUM_TAG)
      return C_mk_bool((double)C_unfix(x) > C_flonum_magnitude(y));

    barf(C_BAD_ARGUMENT_TYPE_NO_NUMBER_ERROR, ">", y);
  }
  else if(!C_immediatep(x) && C_block_header(x) == C_FLONUM_TAG) {
    if(y & C_FIXNUM_BIT) return C_mk_bool(C_flonum_magnitude(x) > (double)C_unfix(y));
    else if(!C_immediatep(y) && C_block_header(y) == C_FLONUM_TAG)
      return C_mk_bool(C_flonum_magnitude(x) > C_flonum_magnitude(y));
    
    barf(C_BAD_ARGUMENT_TYPE_NO_NUMBER_ERROR, ">", y);
  }
  else barf(C_BAD_ARGUMENT_TYPE_NO_NUMBER_ERROR, ">", x);

  return C_SCHEME_FALSE;
}


void C_ccall C_lessp(C_word c, C_word *av)
{
  C_word 
    /* closure = av[ 0 ] */
    k = av[ 1 ],
    x, i2, f, fflag, ilast;
  double flast, f2;

  c -= 2; 
  f = 1;
  av += 2;

  if(c == 0) goto cont;
  
  x = *(av++);

  if(x &C_FIXNUM_BIT) {
    fflag = 0;
    ilast = C_unfix(x);
  }
  else if(!C_immediatep(x) && C_block_header(x) == C_FLONUM_TAG) {
    fflag = 1;
    flast = C_flonum_magnitude(x);
  }
  else barf(C_BAD_ARGUMENT_TYPE_ERROR, "<", x);

  while(--c) {
    x = *(av++);
    
    if(x &C_FIXNUM_BIT) {
      if(fflag) {
	f = flast < (f2 = (double)C_unfix(x));
        flast = f2;
      }
      else {
	f = ilast < (i2 = C_unfix(x));
	ilast = i2;
      }
    }
    else if(!C_immediatep(x) && C_block_header(x) == C_FLONUM_TAG) {
      if(fflag) {
	f = flast < (f2 = C_flonum_magnitude(x));
	flast = f2;
      }
      else {
	f = (double)ilast < (f2 = C_flonum_magnitude(x));
	flast = f2;
	fflag = 1;
      }
    }
    else barf(C_BAD_ARGUMENT_TYPE_ERROR, "<", x);

    if(!f) break;
  }

 cont:
  C_kontinue(k, C_mk_bool(f));
}


C_regparm C_word C_fcall C_i_lessp(C_word x, C_word y)
{
  if(x & C_FIXNUM_BIT) {
    if(y & C_FIXNUM_BIT) return C_mk_bool(x < y);
    else if(!C_immediatep(y) && C_block_header(y) == C_FLONUM_TAG)
      return C_mk_bool((double)C_unfix(x) < C_flonum_magnitude(y));

    barf(C_BAD_ARGUMENT_TYPE_NO_NUMBER_ERROR, "<", y);
  }
  else if(!C_immediatep(x) && C_block_header(x) == C_FLONUM_TAG) {
    if(y & C_FIXNUM_BIT) return C_mk_bool(C_flonum_magnitude(x) < (double)C_unfix(y));
    else if(!C_immediatep(y) && C_block_header(y) == C_FLONUM_TAG)
      return C_mk_bool(C_flonum_magnitude(x) < C_flonum_magnitude(y));
    
    barf(C_BAD_ARGUMENT_TYPE_NO_NUMBER_ERROR, "<", y);
  }
  else barf(C_BAD_ARGUMENT_TYPE_NO_NUMBER_ERROR, "<", x);

  return C_SCHEME_FALSE;
}


void C_ccall C_greater_or_equal_p(C_word c, C_word *av)
{
  C_word 
    /* closure = av[ 0 ] */
    k = av[ 1 ],
    x, i2, f, fflag, ilast;
  double flast, f2;

  c -= 2; 
  f = 1;
  av += 2;

  if(c == 0) goto cont;
  
  x = *(av++);

  if(x &C_FIXNUM_BIT) {
    fflag = 0;
    ilast = C_unfix(x);
  }
  else if(!C_immediatep(x) && C_block_header(x) == C_FLONUM_TAG) {
    fflag = 1;
    flast = C_flonum_magnitude(x);
  }
  else barf(C_BAD_ARGUMENT_TYPE_ERROR, ">=", x);

  while(--c) {
    x = *(av++);
    
    if(x &C_FIXNUM_BIT) {
      if(fflag) {
	f = flast >= (f2 = (double)C_unfix(x));
        flast = f2;
      }
      else {
	f = ilast >= (i2 = C_unfix(x));
	ilast = i2;
      }
    }
    else if(!C_immediatep(x) && C_block_header(x) == C_FLONUM_TAG) {
      if(fflag) {
	f = flast >= (f2 = C_flonum_magnitude(x));
	flast = f2;
      }
      else {
	f = (double)ilast >= (f2 = C_flonum_magnitude(x));
	flast = f2;
	fflag = 1;
      }
    }
    else barf(C_BAD_ARGUMENT_TYPE_ERROR, ">=", x);

    if(!f) break;
  }

 cont:
  C_kontinue(k, C_mk_bool(f));
}


C_regparm C_word C_fcall C_i_greater_or_equalp(C_word x, C_word y)
{
  if(x & C_FIXNUM_BIT) {
    if(y & C_FIXNUM_BIT) return C_mk_bool(x >= y);
    else if(!C_immediatep(y) && C_block_header(y) == C_FLONUM_TAG)
      return C_mk_bool((double)C_unfix(x) >= C_flonum_magnitude(y));

    barf(C_BAD_ARGUMENT_TYPE_NO_NUMBER_ERROR, ">=", y);
  }
  else if(!C_immediatep(x) && C_block_header(x) == C_FLONUM_TAG) {
    if(y & C_FIXNUM_BIT) return C_mk_bool(C_flonum_magnitude(x) >= (double)C_unfix(y));
    else if(!C_immediatep(y) && C_block_header(y) == C_FLONUM_TAG)
      return C_mk_bool(C_flonum_magnitude(x) >= C_flonum_magnitude(y));
    
    barf(C_BAD_ARGUMENT_TYPE_NO_NUMBER_ERROR, ">=", y);
  }
  else barf(C_BAD_ARGUMENT_TYPE_NO_NUMBER_ERROR, ">=", x);

  return C_SCHEME_FALSE;
}


void C_ccall C_less_or_equal_p(C_word c, C_word *av)
{
  C_word 
    /* closure = av[ 0 ] */
    k = av[ 1 ],
    x, i2, f, fflag, ilast;
  double flast, f2;

  c -= 2; 
  f = 1;
  av += 2;

  if(c == 0) goto cont;
  
  x = *(av++);

  if(x &C_FIXNUM_BIT) {
    fflag = 0;
    ilast = C_unfix(x);
  }
  else if(!C_immediatep(x) && C_block_header(x) == C_FLONUM_TAG) {
    fflag = 1;
    flast = C_flonum_magnitude(x);
  }
  else barf(C_BAD_ARGUMENT_TYPE_ERROR, "<=", x);

  while(--c) {
    x = *(av++);
    
    if(x &C_FIXNUM_BIT) {
      if(fflag) {
	f = flast <= (f2 = (double)C_unfix(x));
        flast = f2;
      }
      else {
	f = ilast <= (i2 = C_unfix(x));
	ilast = i2;
      }
    }
    else if(!C_immediatep(x) && C_block_header(x) == C_FLONUM_TAG) {
      if(fflag) {
	f = flast <= (f2 = C_flonum_magnitude(x));
	flast = f2;
      }
      else {
	f = (double)ilast <= (f2 = C_flonum_magnitude(x));
	flast = f2;
	fflag = 1;
      }
    }
    else barf(C_BAD_ARGUMENT_TYPE_ERROR, "<=", x);

    if(!f) break;
  }

 cont:
  C_kontinue(k, C_mk_bool(f));
}


C_regparm C_word C_fcall C_i_less_or_equalp(C_word x, C_word y)
{
  if(x & C_FIXNUM_BIT) {
    if(y & C_FIXNUM_BIT) return C_mk_bool(x <= y);
    else if(!C_immediatep(y) && C_block_header(y) == C_FLONUM_TAG)
      return C_mk_bool((double)C_unfix(x) <= C_flonum_magnitude(y));

    barf(C_BAD_ARGUMENT_TYPE_NO_NUMBER_ERROR, "<=", y);
  }
  else if(!C_immediatep(x) && C_block_header(x) == C_FLONUM_TAG) {
    if(y & C_FIXNUM_BIT) return C_mk_bool(C_flonum_magnitude(x) <= (double)C_unfix(y));
    else if(!C_immediatep(y) && C_block_header(y) == C_FLONUM_TAG)
      return C_mk_bool(C_flonum_magnitude(x) <= C_flonum_magnitude(y));
    
    barf(C_BAD_ARGUMENT_TYPE_NO_NUMBER_ERROR, "<=", y);
  }
  else barf(C_BAD_ARGUMENT_TYPE_NO_NUMBER_ERROR, "<=", x);

  return C_SCHEME_FALSE;
}


void C_ccall C_expt(C_word c, C_word *av)
{
  C_word
    /* closure = av[ 0 ] */
    k = av[ 1 ], 
    n1, n2, r;
  double m1, m2;
  C_alloc_flonum;

  if(c != 4) C_bad_argc(c, 4);

  n1 = av[ 2 ];
  n2 = av[ 3 ];

  if(n1 & C_FIXNUM_BIT) m1 = C_unfix(n1);
  else if(!C_immediatep(n1) && C_block_header(n1) == C_FLONUM_TAG)
    m1 = C_flonum_magnitude(n1);
  else barf(C_BAD_ARGUMENT_TYPE_ERROR, "expt", n1);

  if(n2 & C_FIXNUM_BIT) m2 = C_unfix(n2);
  else if(!C_immediatep(n2) && C_block_header(n2) == C_FLONUM_TAG)
    m2 = C_flonum_magnitude(n2);
  else barf(C_BAD_ARGUMENT_TYPE_ERROR, "expt", n2);

  m1 = pow(m1, m2);
  r = (C_word)m1;

  if(r == m1 && (n1 & C_FIXNUM_BIT) && (n2 & C_FIXNUM_BIT) && modf(m1, &m2) == 0.0 && C_fitsinfixnump(r))
    C_kontinue(k, C_fix(r));

  C_kontinue_flonum(k, m1);
}


void C_ccall C_gc(C_word c, C_word *av)
{
  C_word
    /* closure = av[ 0 ] */
    k = av[ 1 ];
  int f;
  C_word 
    arg, *p,
    size = 0;

  if(c == 3) {
    arg = av[ 2 ];
    f = C_truep(arg);
  }
  else if(c != 2) C_bad_min_argc(c, 2);
  else f = 1;

  C_save(k);
  p = C_temporary_stack;

  if(c == 3) {
    if((arg & C_FIXNUM_BIT) != 0) size = C_unfix(arg);
    else if(arg == C_SCHEME_END_OF_LIST) size = percentage(heap_size, C_heap_growth);
  }

  if(size && !C_heap_size_is_fixed) {
    C_rereclaim2(size, 0);
    C_temporary_stack = C_temporary_stack_bottom;
    gc_2(0, p);
  }
  else if(f) C_fromspace_top = C_fromspace_limit;

  C_reclaim((void *)gc_2, 1);
}


void C_ccall gc_2(C_word c, C_word *av)
{
  C_word k = av[ 0 ];
  
  C_kontinue(k, C_fix((C_uword)C_fromspace_limit - (C_uword)C_fromspace_top));
}


void C_ccall C_open_file_port(C_word c, C_word *av)
{
  C_word
    /* closure = av[ 0 ] */
    k = av[ 1 ], 
    port = av[ 2 ],
    channel = av[ 3 ],
    mode = av[ 4 ];
  C_FILEPTR fp = (C_FILEPTR)NULL;
  C_char fmode[ 4 ];
  C_word n;
  char *buf;

  switch(channel) {
  case C_fix(0): fp = C_stdin; break;
  case C_fix(1): fp = C_stdout; break;
  case C_fix(2): fp = C_stderr; break;
  default:
    n = C_header_size(channel);
    buf = buffer;

    if(n >= STRING_BUFFER_SIZE) {
      if((buf = (char *)C_malloc(n + 1)) == NULL)
	barf(C_OUT_OF_MEMORY_ERROR, "open");
    }

    C_strncpy(buf, C_c_string(channel), n);
    buf[ n ] = '\0';
    if (n != strlen(buf))
      barf(C_ASCIIZ_REPRESENTATION_ERROR, "open", channel);
    n = C_header_size(mode);
    if (n >= sizeof(fmode)) n = sizeof(fmode) - 1;
    C_strncpy(fmode, C_c_string(mode), n);
    fmode[ n ] = '\0';
    if (n != strlen(fmode)) /* Shouldn't happen, but never hurts */
      barf(C_ASCIIZ_REPRESENTATION_ERROR, "open", mode);
    fp = C_fopen(buf, fmode);

    if(buf != buffer) C_free(buf);
  }
  
  C_set_block_item(port, 0, (C_word)fp);
  C_kontinue(k, C_mk_bool(fp != NULL));
}


void C_ccall C_allocate_vector(C_word c, C_word *av)
{
  C_word 
    /* closure = av[ 0 ] */
    k = av[ 1 ],
    size, bvecf, init, align8,
    bytes,
    n, *p;

  if(c != 6) C_bad_argc(c, 6);

  size = av[ 2 ];
  bvecf = av[ 3 ];
  init = av[ 4 ];
  align8 = av[ 5 ];
  n = C_unfix(size);

  if(n > C_HEADER_SIZE_MASK || n < 0)
    barf(C_OUT_OF_RANGE_ERROR, NULL, size, C_fix(C_HEADER_SIZE_MASK));

  if(!C_truep(bvecf)) bytes = C_wordstobytes(n) + sizeof(C_word);
  else bytes = n + sizeof(C_word);

  if(C_truep(align8)) bytes += sizeof(C_word);

  C_save(k);
  C_save(size);
  C_save(init);
  C_save(bvecf);
  C_save(align8);
  C_save(C_fix(bytes));

  if(!C_demand(C_bytestowords(bytes))) {
    /* Allocate on heap: */
    if((C_uword)(C_fromspace_limit - C_fromspace_top) < (bytes + stack_size * 2))
      C_fromspace_top = C_fromspace_limit; /* trigger major GC */
  
    C_save(C_SCHEME_TRUE);
    /* We explicitly pass 7 here, that's the number of things saved.
     * That's the arguments, plus one additional thing: the mode.
     */
    C_reclaim((void *)allocate_vector_2, 7);
  }

  C_save(C_SCHEME_FALSE);
  p = C_temporary_stack;
  C_temporary_stack = C_temporary_stack_bottom;
  allocate_vector_2(0, p);
}


void C_ccall allocate_vector_2(C_word c, C_word *av)
{
  C_word 
    mode = av[ 0 ],
    bytes = C_unfix(av[ 1 ]),
    align8 = av[ 2 ],
    bvecf = av[ 3 ],
    init = av[ 4 ],
    size = C_unfix(av[ 5 ]),
    k = av[ 6 ],
    *v0, v;

  if(C_truep(mode)) {
    while((C_uword)(C_fromspace_limit - C_fromspace_top) < (bytes + stack_size)) {
      if(C_heap_size_is_fixed)
	panic(C_text("out of memory - cannot allocate vector (heap resizing disabled)"));

      C_save(init);
      C_save(k);
      C_rereclaim2(percentage(heap_size, C_heap_growth) + (C_uword)bytes, 0);
      k = C_restore;
      init = C_restore;
    }

    v0 = (C_word *)C_align((C_word)C_fromspace_top);
    C_fromspace_top += C_align(bytes);
  }
  else v0 = C_alloc(C_bytestowords(bytes));

#ifndef C_SIXTY_FOUR
  if(C_truep(align8) && C_aligned8(v0)) ++v0;
#endif

  v = (C_word)v0;

  if(!C_truep(bvecf)) {
    *(v0++) = C_VECTOR_TYPE | size | (C_truep(align8) ? C_8ALIGN_BIT : 0);
  
    while(size--) *(v0++) = init;
  }
  else {
    *(v0++) = C_STRING_TYPE | size;

    if(C_truep(init))
      C_memset(v0, C_character_code(init), size);
  }

  C_kontinue(k, v);
}


void C_ccall C_string_to_symbol(C_word c, C_word *av) 
{ 
  C_word
    /* closure = av[ 0 ] */
    k = av[ 1 ],
    string;
  int len, key;
  C_word s, *a = C_alloc(C_SIZEOF_SYMBOL + C_SIZEOF_BUCKET);
  C_char *name;

  if(c != 3) C_bad_argc(c, 3);

  string = av[ 2 ];

  if(C_immediatep(string) || C_header_bits(string) != C_STRING_TYPE)
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "string->symbol", string);
    
  len = C_header_size(string);
  name = (C_char *)C_data_pointer(string);
  key = hash_string(len, name, symbol_table->size, symbol_table->rand, 0);

  if(!C_truep(s = lookup(key, len, name, symbol_table))) 
    s = add_symbol(&a, key, string, symbol_table);

  C_kontinue(k, s);
}


void C_ccall C_flonum_fraction(C_word c, C_word *av) 
{
  C_word
    /* closure = av[ 0 ] */
    k = av[ 1 ],
    n = av[ 2 ];
  double i, fn = C_flonum_magnitude(n);
  C_alloc_flonum;

  C_kontinue_flonum(k, modf(fn, &i));
}


void C_ccall C_flonum_rat(C_word c, C_word *av)
{
  C_word
    /* closure = av[ 0 ] */
    k = av[ 1 ],
    n = av[ 2 ];
  double frac, tmp, numer, denom, fn = C_flonum_magnitude(n);
  double ga, gb;
  C_word ab[WORDS_PER_FLONUM * 2], *ap = ab;
  int i = 0;
  C_word av2[ 4 ];

  if (isnormal(fn)) {
    /* Calculate bit-length of the fractional part (ie, after decimal point) */
    frac = fn;
    while(!C_isnan(frac) && !C_isinf(frac) && C_modf(frac, &tmp) != 0.0) {
      frac *= 2;
      if (i++ > 3000) /* should this be flonum-maximum-exponent? */
        barf(C_CANT_REPRESENT_INEXACT_ERROR, "fprat", n);
    }
    
    /* Now we can compute the rational number r = 2^i/X*2^i = numer/denom. */
    denom = pow(2, i);
    numer = fn*denom;
  } else { /* denormalised/subnormal number: [+-]1.0/+inf.0 */
    numer = fn > 0.0 ? 1.0 : -1.0;
    denom = 1.0/0.0; /* +inf */
  }

  av2[ 0 ] = C_SCHEME_UNDEFINED;
  av2[ 1 ] = k;
  av2[ 2 ] = C_flonum(&ap, numer);
  av2[ 3 ] = C_flonum(&ap, denom);
  C_values(4, av2);
}


C_regparm C_word C_fcall 
C_a_i_exact_to_inexact(C_word **a, int c, C_word n)
{
  if(n & C_FIXNUM_BIT) 
    return C_flonum(a, (double)C_unfix(n));
  else if(C_immediatep(n) || C_block_header(n) != C_FLONUM_TAG)
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "exact->inexact", n);

  return n;
}


/* this is different from C_a_i_flonum_round, for R5RS compatibility */
C_regparm C_word C_fcall C_a_i_flonum_round_proper(C_word **ptr, int c, C_word n)
{
  double fn, i, f, i2, r;

  fn = C_flonum_magnitude(n);
  if(fn < 0.0) {
    f = modf(-fn, &i);
    if(f < 0.5 || (f == 0.5 && modf(i * 0.5, &i2) == 0.0))
      r = -i;
    else
      r = -(i + 1.0);
  }
  else if(fn == 0.0/* || fn == -0.0*/)
    r = fn;
  else {
    f = modf(fn, &i);
    if(f < 0.5 || (f == 0.5 && modf(i * 0.5, &i2) == 0.0))
      r = i;
    else
      r = i + 1.0;
  }

  return C_flonum(ptr, r);
}


void C_ccall C_quotient(C_word c, C_word *av)
{
  C_word
    /* closure = av[ 0 ] */
    k = av[ 1 ],
    n1, n2;
  double f1, f2, r;
  C_word result;
  C_alloc_flonum;

  if(c != 4) C_bad_argc(c, 4);

  n1 = av[ 2 ];
  n2 = av[ 3 ];

  if(n1 &C_FIXNUM_BIT) {
    if(n2 &C_FIXNUM_BIT) {
      if((n2 = C_unfix(n2)) == 0)
	barf(C_DIVISION_BY_ZERO_ERROR, "quotient");
      
      result = C_fix(C_unfix(n1) / n2);
      C_kontinue(k, result);
    }
    else if(!C_immediatep(n2) && C_block_header(n2) == C_FLONUM_TAG) {
      f1 = (double)C_unfix(n1);
      f2 = C_flonum_magnitude(n2);
      if(C_isnan(f2) || C_isinf(f2) || C_modf(f2, &r) != 0.0)
        barf(C_BAD_ARGUMENT_TYPE_NO_INTEGER_ERROR, "quotient", n2);
    }
    else barf(C_BAD_ARGUMENT_TYPE_ERROR, "quotient", n2);
  }
  else if(!C_immediatep(n1) && C_block_header(n1) == C_FLONUM_TAG) {
    f1 = C_flonum_magnitude(n1);
    if(C_isnan(f1) || C_isinf(f1) || C_modf(f1, &r) != 0.0)
      barf(C_BAD_ARGUMENT_TYPE_NO_INTEGER_ERROR, "quotient", n1);

    if(n2 &C_FIXNUM_BIT)
      f2 = (double)C_unfix(n2);
    else if(!C_immediatep(n2) && C_block_header(n2) == C_FLONUM_TAG) {
      f2 = C_flonum_magnitude(n2);
      if(C_isnan(f2) || C_isinf(f2) || C_modf(f2, &r) != 0.0)
        barf(C_BAD_ARGUMENT_TYPE_NO_INTEGER_ERROR, "quotient", n2);
    }
    else barf(C_BAD_ARGUMENT_TYPE_ERROR, "quotient", n2);
  }
  else barf(C_BAD_ARGUMENT_TYPE_ERROR, "quotient", n1);

  if(f2 == 0)
    barf(C_DIVISION_BY_ZERO_ERROR, "quotient");

  modf(f1 / f2, &r);
  C_kontinue_flonum(k, r);
}


C_regparm C_word C_fcall
C_a_i_string_to_number(C_word **a, int c, C_word str, C_word radix0)
{
  int radix, radixpf = 0, sharpf = 0, ratf = 0, exactf = 0, exactpf = 0, periodf = 0, expf = 0;
  C_word n1, n;
  C_char *sptr, *eptr, *rptr;
  double fn1, fn;

  if(radix0 & C_FIXNUM_BIT) radix = C_unfix(radix0);
  else barf(C_BAD_ARGUMENT_TYPE_BAD_BASE_ERROR, "string->number", radix0);

  if (radix < 2 || radix > 36) /* Makes no sense and isn't supported */
    barf(C_BAD_ARGUMENT_TYPE_BAD_BASE_ERROR, "string->number", radix0);

  if(C_immediatep(str) || C_header_bits(str) != C_STRING_TYPE)
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "string->number", str);

  if((n = C_header_size(str)) == 0) {
  fail:
    n = C_SCHEME_FALSE;
    goto fini;
  }

  if(n >= STRING_BUFFER_SIZE - 1) goto fail;

  C_memcpy(sptr = buffer, C_c_string(str), n > (STRING_BUFFER_SIZE - 1) ? STRING_BUFFER_SIZE : n);
  buffer[ n ] = '\0';
  if (n != strlen(buffer)) /* Don't barf; this is simply invalid number syntax */
    goto fail;
  
  while(*sptr == '#') {
    switch(C_tolower((int)*(++sptr))) {
    case 'b': if(radixpf) goto fail; else { radix = 2; radixpf = 1; } break;
    case 'o': if(radixpf) goto fail; else { radix = 8; radixpf = 1; } break;
    case 'd': if(radixpf) goto fail; else { radix = 10; radixpf = 1; } break;
    case 'x': if(radixpf) goto fail; else { radix = 16; radixpf = 1; } break;
    case 'e': if(exactpf) goto fail; else { exactf = 1; exactpf = 1; } break;
    case 'i': if(exactpf) goto fail; else { exactf = 0; exactpf = 1; } break;
    default: goto fail;  /* Unknown prefix type */
    }

    ++sptr;
  }
  
  /* Scan for embedded special characters and do basic sanity checking: */
  for(eptr = sptr, rptr = sptr; *eptr != '\0'; ++eptr) {
    switch(C_tolower((int)*eptr)) {
    case '.': 
      if(periodf || ratf || expf) goto fail;
      
      periodf = 1;
      break;

    case '#':
      if (expf || (eptr == rptr) ||
	  (!sharpf && (eptr == rptr+1) && (C_strchr("+-.", *rptr) != NULL)))
        goto fail;
      
      sharpf = 1;
      *eptr = '0';
      
      break;
    case '/':
      if(periodf || ratf || expf || eptr == sptr) goto fail;
      
      sharpf = 0; /* Allow sharp signs in the denominator */
      ratf = 1;
      rptr = eptr+1;
      break;
    case 'e':
    case 'd':
    case 'f':
    case 'l':
    case 's':
      /* Don't set exp flag if we see the "f" in "inf.0" (preceded by 'n') */
      /* Other failure modes are handled elsewhere. */
      if(radix == 10 && eptr > sptr && C_tolower((int)*(eptr-1)) != 'n') {
        if (ratf) goto fail;
	
        expf = 1;
	sharpf = 0;
	*eptr = 'e'; /* strtod() normally only understands 'e', not dfls */
      }
      break;
    default:
      if(sharpf) goto fail;
      break;
    }
  }
  if (eptr == rptr) goto fail; /* Disallow "empty" numbers like "#x" and "1/" */
  
  /* check for rational representation: */
  if(rptr != sptr) {
    if (*(rptr) == '-' || *(rptr) == '+') {
      n = C_SCHEME_FALSE;
      goto fini;
    }
    *(rptr-1) = '\0';

    switch(convert_string_to_number(sptr, radix, &n1, &fn1)) {
    case 0:
      n = C_SCHEME_FALSE;
      goto fini;

    case 1:
      fn1 = (double)n1;
      break;

      /* case 2: nop */
    }

    sptr = rptr;
  }    
  
  /* convert number and return result: */
  switch(convert_string_to_number(sptr, radix, &n, &fn)) {
  case 0: 			/* failed */
    n = C_SCHEME_FALSE;
    break;

  case 1:			/* fixnum */
    if(sharpf || ratf || (exactpf && !exactf)) {
      n = C_flonum(a, ratf ? fn1 / (double)n : (double)n);

      if(exactpf && exactf) n = maybe_inexact_to_exact(n);
    }
    else n = C_fix(n);

    break;

  case 2:			/* flonum */
    n = C_flonum(a, ratf ? fn1 / fn : fn);

    if(exactpf && exactf) n = maybe_inexact_to_exact(n);

    break;
  }

 fini:
  return n;
}


static int from_n_nary(C_char *str, int base, double *r)
{
  double n = 0;
  C_char *ptr = str;

  while(*ptr != '\0') {
    int c = C_tolower((int)(*(ptr++)));

    if(c < '0') return 0;
    else if(c >= '0' + base) {
      if(base < 10) return 0;
      else if(c < 'a') return 0;
      else if(c >= 'a' + base - 10) return 0;
      else n = n * base + c - 'a' + 10;
    }
    else n = n * base + c - '0';
  }

  *r = n;
  return 1;
}


C_regparm C_word C_fcall convert_string_to_number(C_char *str, int radix, C_word *fix, double *flo)
{
  C_ulong ln;
  C_word n;
  C_char *eptr, *eptr2;
  double fn;
  int len = C_strlen(str);

  if(radix == 10) {
    if (len == 6) {
      if((*str == '+' || *str == '-') &&
         C_strchr("inIN", *(str+1)) != NULL &&
         C_strchr("naNA", *(str+2)) != NULL &&
         C_strchr("fnFN", *(str+3)) != NULL &&
         *(str+4) == '.' && *(str+5) == '0') {
        if (*(str+1) == 'i' || *(str+1) == 'I')   /* Inf */
          *flo = 1.0/0.0;
        else                                      /* NaN */
          *flo = 0.0/0.0;
        if (*str == '-')
          *flo *= -1.0;
        return 2;
      }
    }
    /* Prevent C parser from accepting things like "-inf" on its own... */
    for(n = 0; n < len; ++n) {
      if (C_strchr("+-0123456789e.", *(str+n)) == NULL)
        return 0;
    }
  }

  if(C_strpbrk(str, "xX\0") != NULL) return 0;

  errno = 0;
  n = C_strtow(str, &eptr, radix);
  
  if(((n == C_LONG_MAX || n == C_LONG_MIN) && errno != 0) || *eptr != '\0') {
    if(radix != 10)
      return from_n_nary(str, radix, flo) ? 2 : 0;

    errno = 0;
    fn = C_strtod(str, &eptr2);

    if((fn == HUGE_VAL && errno != 0) || fn == -HUGE_VAL) return 0;
    else if(eptr2 == str) return 0;
    else if(*eptr2 == '\0' || (eptr != eptr2 && !C_strncmp(eptr2, ".0", C_strlen(eptr2)))) {
      *flo = fn;
      return 2;
    }

    return 0;
  }
  else if((n & C_INT_SIGN_BIT) != (((C_uword)n << 1) & C_INT_SIGN_BIT)) { /* doesn't fit into fixnum? */
    if(*eptr == '\0' || !C_strncmp(eptr, ".0", C_strlen(eptr))) {
      *flo = (double)n;
      return 2;
    }
    else return 0;
  }
  else {
    *fix = n;
    return 1;
  }
}


static char *to_n_nary(C_uword num, C_uword base)
{
  char *p;
  static char digits[] ={ '0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F' };
  buffer [ 66 ] = '\0';
  p = buffer + 66;

  do {
    *(--p) = digits [ num % base ];
    num /= base;
  } while (num);

  return p;
}


void C_ccall C_number_to_string(C_word c, C_word *av)
{
  C_word
    /* closure * av[ 0 ] */
    k = av[ 1 ],
    num,
    radix, *a;
  C_char *p;
  double f;
  int neg = 0;

  if(c == 3) radix = 10;
  else if(c == 4) {
    radix = av[ 3 ];
    
    if(radix & C_FIXNUM_BIT) radix = C_unfix(radix);
    else barf(C_BAD_ARGUMENT_TYPE_BAD_BASE_ERROR, "number->string", radix);
  }
  else C_bad_argc(c, 3);

  num = av[ 2 ];

  if(num & C_FIXNUM_BIT) {
    num = C_unfix(num);

    if(num < 0) {
      neg = 1;
      num = -num;
    }

    if((radix < 2) || (radix > 16)){
      barf(C_BAD_ARGUMENT_TYPE_BAD_BASE_ERROR, "number->string", C_fix(radix));
    }

    switch(radix) {
#ifdef C_SIXTY_FOUR
    case 8: C_snprintf(p = buffer + 1, sizeof(buffer) -1 , C_text("%llo"), (long long)num); break;
    case 10: C_snprintf(p = buffer + 1, sizeof(buffer) - 1, C_text("%lld"), (long long)num); break;
    case 16: C_snprintf(p = buffer + 1, sizeof(buffer) - 1, C_text("%llx"), (long long)num); break;
#else
    case 8: C_snprintf(p = buffer + 1, sizeof(buffer) - 1, C_text("%o"), num); break;
    case 10: C_snprintf(p = buffer + 1, sizeof(buffer) - 1, C_text("%d"), num); break;
    case 16: C_snprintf(p = buffer + 1, sizeof(buffer) - 1, C_text("%x"), num); break;
#endif
    default: 
      p = to_n_nary(num, radix);
    }
  }
  else if(!C_immediatep(num) && C_block_header(num) == C_FLONUM_TAG) {
    f = C_flonum_magnitude(num);

    if(C_fits_in_unsigned_int_p(num) == C_SCHEME_TRUE) {
      if(f < 0) {
	neg = 1;
	f = -f;
      }

      if((radix < 2) || (radix > 16)){
	barf(C_BAD_ARGUMENT_TYPE_BAD_BASE_ERROR, "number->string", C_fix(radix));
      }

      switch(radix) {
      case 8:
	C_snprintf(p = buffer, sizeof(buffer), "%o", (unsigned int)f);
	goto fini;

      case 16:
	C_snprintf(p = buffer, sizeof(buffer), "%x", (unsigned int)f);
	goto fini;

      case 10: break;		/* force output of decimal point to retain
				   read/write invariance (the little we support) */

      default:
	p = to_n_nary((unsigned int)f, radix);
	goto fini;

      }
    } 

    if(C_isnan(f)) {
      C_strlcpy(buffer, C_text("+nan.0"), sizeof(buffer));
      p = buffer;
      goto fini;
    }
    else if(C_isinf(f)) {
      C_snprintf(buffer, sizeof(buffer), "%cinf.0", f > 0 ? '+' : '-');
      p = buffer;
      goto fini;
    }

    C_snprintf(buffer, STRING_BUFFER_SIZE, C_text("%.*g"),
	       flonum_print_precision, f);
    buffer[STRING_BUFFER_SIZE-1] = '\0';

    if((p = C_strpbrk(buffer, C_text(".eE"))) == NULL) {
      if(*buffer == 'i' || *buffer == 'n') { /* inf or nan */
	C_memmove(buffer + 1, buffer, C_strlen(buffer) + 1);
	*buffer = '+';
      }
      else if(buffer[ 1 ] != 'i') C_strlcat(buffer, C_text(".0"), sizeof(buffer)); /* negative infinity? */
    }

    p = buffer;
  }
  else
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "number->string", num);

 fini:
  if(neg) *(--p) = '-';

  radix = C_strlen(p);
  a = C_alloc((C_bytestowords(radix) + 1));
  radix = C_string(&a, radix, p);
  C_kontinue(k, radix);
}


/* special case for fixnum arg and decimal radix */
void C_ccall C_fixnum_to_string(C_word c, C_word *av)
{
  C_word
    /* closure = av[ 0 ] */
    k = av[ 1 ],
    num = av[ 2 ],
    *a, s;
  int n;

  /*XXX is this necessary? */
#ifdef C_SIXTY_FOUR
  C_snprintf(buffer, sizeof(buffer), C_text(LONG_FORMAT_STRING), C_unfix(num));
#else
  C_snprintf(buffer, sizeof(buffer), C_text("%d"), C_unfix(num));
#endif
  n = C_strlen(buffer);
  a = C_alloc(C_bytestowords(n) + 1);
  s = C_string2(&a, buffer);
  C_kontinue(k, s);
}


void C_ccall C_make_structure(C_word c, C_word *av)
{
  C_word
    /* closure = av[ 0 ] */
    k = av[ 1 ],
    type = av[ 2 ],
    size = c - 3,
    *s, s0;

  if(!C_demand(size + 2))
    C_save_and_reclaim((void *)C_make_structure, c, av);

  s = C_alloc(size + 2),
  s0 = (C_word)s;
  *(s++) = C_STRUCTURE_TYPE | (size + 1);
  *(s++) = type;
  av += 3;

  while(size--)
    *(s++) = *(av++);

  C_kontinue(k, s0);
}


void C_ccall C_make_symbol(C_word c, C_word *av)
{
  C_word
    /* closure = av[ 0 ] */
    k = av[ 1 ],
    name = av[ 2 ],
    ab[ C_SIZEOF_SYMBOL ], 
    *a = ab,
    s0 = (C_word)a;

  *(a++) = C_SYMBOL_TYPE | (C_SIZEOF_SYMBOL - 1);
  *(a++) = C_SCHEME_UNBOUND;
  *(a++) = name;
  *a = C_SCHEME_END_OF_LIST;
  C_kontinue(k, s0);
}


void C_ccall C_make_pointer(C_word c, C_word *av)
{
  C_word
    /* closure = av[ 0 ] */
    k = av[ 1 ],
    ab[ 2 ], 
    *a = ab,
    p;

  p = C_mpointer(&a, NULL);
  C_kontinue(k, p);
}


void C_ccall C_make_tagged_pointer(C_word c, C_word *av)
{
  C_word
    /* closure = av[ 0 ] */
    k = av[ 1 ],
    tag = av[ 2 ],
    ab[ 3 ],
    *a = ab,
    p;

  p = C_taggedmpointer(&a, tag, NULL);
  C_kontinue(k, p);
}


void C_ccall C_ensure_heap_reserve(C_word c, C_word *av) 
{
  C_word
    /* closure = av[ 0 ] */
    k = av[ 1 ],
    n = av[ 2 ],
    *p;
  
  C_save(k);

  if(!C_demand(C_bytestowords(C_unfix(n))))
    C_reclaim((void *)generic_trampoline, 1);

  p = C_temporary_stack;
  C_temporary_stack = C_temporary_stack_bottom;
  generic_trampoline(0, p);
}


void C_ccall generic_trampoline(C_word c, C_word *av)
{
  C_word k = av[ 0 ];

  C_kontinue(k, C_SCHEME_UNDEFINED);
}


void C_ccall C_return_to_host(C_word c, C_word *av)
{
  C_word
    /* closure = av[ 0 ] */
    k = av[ 1 ];

  return_to_host = 1;
  C_save(k);
  C_reclaim((void *)generic_trampoline, 1);
}


void C_ccall C_get_symbol_table_info(C_word c, C_word *av)
{
  C_word
    /* closure = av[ 0 ] */
    k = av[ 1 ];
  double d1, d2;
  int n = 0, total;
  C_SYMBOL_TABLE *stp;
  C_word
    x, y,
    ab[ WORDS_PER_FLONUM * 2 + 5 ], /* 2 flonums + 1 vector of 4 elements */
    *a = ab;

  for(stp = symbol_table_list; stp != NULL; stp = stp->next)
    ++n;
  
  d1 = compute_symbol_table_load(&d2, &total);
  x = C_flonum(&a, d1);		/* load */
  y = C_flonum(&a, d2);		/* avg bucket length */
  C_kontinue(k, C_vector(&a, 4, x, y, C_fix(total), C_fix(n)));
}


void C_ccall C_get_memory_info(C_word c, C_word *av)
{
  C_word
    /* closure = av[ 0 ] */
    k = av[ 1 ],
    ab[ 3 ],
    *a = ab;

  C_kontinue(k, C_vector(&a, 2, C_fix(heap_size), C_fix(stack_size)));
}


void C_ccall C_context_switch(C_word c, C_word *av)
{
  C_word
    /* closure = av[ 0 ] */
    state = av[ 2 ],
    n = C_header_size(state) - 1,
    adrs = C_block_item(state, 0),
    *av2;
  C_proc tp = (C_proc)C_block_item(adrs,0);

  /* Copy argvector because it may be mutated in-place.  The state
   * vector should not be re-invoked(?), but it can be kept alive
   * during GC, so the mutated argvector/state slots may turn stale.
   */
  av2 = C_alloc(n);
  C_memcpy(av2, (C_word *)state + 2, n * sizeof(C_word));
  tp(n, av2);
}


void C_ccall C_peek_signed_integer(C_word c, C_word *av)
{
  C_word
    /* closure = av[ 0 ] */
    k = av[ 1 ],
    v = av[ 2 ],
    index = av[ 3 ],
    x = C_block_item(v, C_unfix(index));
  C_alloc_flonum;

  if((x & C_INT_SIGN_BIT) != (((C_uword)x << 1) & C_INT_SIGN_BIT)) {
    C_kontinue_flonum(k, (double)x);
  }

  C_kontinue(k, C_fix(x));
}


void C_ccall C_peek_unsigned_integer(C_word c, C_word *av)
{
  C_word
    /* closure = av[ 0 ] */
    k = av[ 1 ],
    v = av[ 2 ],
    index = av[ 3 ],
    x = C_block_item(v, C_unfix(index));
  C_alloc_flonum;

  if((x & C_INT_SIGN_BIT) || (((C_uword)x << 1) & C_INT_SIGN_BIT)) {
    C_kontinue_flonum(k, (double)(C_uword)x);
  }

  C_kontinue(k, C_fix(x));
}


void C_ccall C_decode_seconds(C_word c, C_word *av)
{
  C_word
    /* closure = av[ 0 ] */
    k = av[ 1 ],
    secs = av[ 2 ],
    mode = av[ 3 ];
  time_t tsecs;
  struct tm *tmt;
  C_word
    ab[ 11 ], 
    *a = ab,
    info;

  tsecs = (time_t)((secs & C_FIXNUM_BIT) != 0 ? C_unfix(secs) : C_flonum_magnitude(secs));
  
  if(mode == C_SCHEME_FALSE) tmt = C_localtime(&tsecs);
  else tmt = C_gmtime(&tsecs);

  if(tmt  == NULL)
    C_kontinue(k, C_SCHEME_FALSE);
  
  info = C_vector(&a, 10, C_fix(tmt->tm_sec), C_fix(tmt->tm_min), C_fix(tmt->tm_hour),
		  C_fix(tmt->tm_mday), C_fix(tmt->tm_mon), C_fix(tmt->tm_year),
		  C_fix(tmt->tm_wday), C_fix(tmt->tm_yday),
		  tmt->tm_isdst > 0 ? C_SCHEME_TRUE : C_SCHEME_FALSE,
#ifdef C_GNU_ENV
                  /* negative for west of UTC, but we want positive */
		  C_fix(-tmt->tm_gmtoff)
#elif defined(__CYGWIN__) || defined(__MINGW32__) || defined(_WIN32) || defined(__WINNT__)
                  C_fix(mode == C_SCHEME_FALSE ? _timezone : 0) /* does not account for DST */
#else
                  C_fix(mode == C_SCHEME_FALSE ? timezone : 0)  /* does not account for DST */
#endif
		  );
  C_kontinue(k, info);
}


void C_ccall C_machine_byte_order(C_word c, C_word *av)
{
  C_word
    /* closure = av[ 0 ] */
    k = av[ 1 ];
  char *str;
  C_word *a, s;

  if(c != 2) C_bad_argc(c, 2);

#if defined(C_MACHINE_BYTE_ORDER)
  str = C_MACHINE_BYTE_ORDER;
#else
  C_cblock
    static C_word one_two_three = 123;
    str = (*((C_char *)&one_two_three) != 123) ? "big-endian" : "little-endian";
  C_cblockend;
#endif

  a = C_alloc(2 + C_bytestowords(strlen(str)));
  s = C_string2(&a, str);

  C_kontinue(k, s);
}


void C_ccall C_machine_type(C_word c, C_word *av)
{
  C_word 
    /* closure = av[ 0 ] */
    k = av[ 1 ],
    *a, s;

  if(c != 2) C_bad_argc(c, 2);

  a = C_alloc(2 + C_bytestowords(strlen(C_MACHINE_TYPE)));
  s = C_string2(&a, C_MACHINE_TYPE);
  
  C_kontinue(k, s);
}


void C_ccall C_software_type(C_word c, C_word *av)
{
  C_word
    /* closure = av[ 0 ] */
    k = av[ 1 ],
    *a, s;

  if(c != 2) C_bad_argc(c, 2);

  a = C_alloc(2 + C_bytestowords(strlen(C_SOFTWARE_TYPE)));
  s = C_string2(&a, C_SOFTWARE_TYPE);

 C_kontinue(k, s);
}


void C_ccall C_build_platform(C_word c, C_word *av)
{
  C_word
    /* closure = av[ 0 ] */
    k = av[ 1 ],
    *a, s;

  if(c != 2) C_bad_argc(c, 2);

  a = C_alloc(2 + C_bytestowords(strlen(C_BUILD_PLATFORM)));
  s = C_string2(&a, C_BUILD_PLATFORM);

 C_kontinue(k, s);
}


void C_ccall C_software_version(C_word c, C_word *av)
{
  C_word
    /* closure = av[ 0 ] */
    k = av[ 1 ],
    *a, s;

  if(c != 2) C_bad_argc(c, 2);

  a = C_alloc(2 + C_bytestowords(strlen(C_SOFTWARE_VERSION)));
  s = C_string2(&a, C_SOFTWARE_VERSION);

 C_kontinue(k, s);
}


/* Register finalizer: */

void C_ccall C_register_finalizer(C_word c, C_word *av)
{
  C_word
    /* closure = av[ 0 ]) */
    k = av[ 1 ],
    x = av[ 2 ],
    proc = av[ 3 ];

  if(C_immediatep(x) || (!C_in_stackp(x) && !C_in_heapp(x))) /* not GCable? */
    C_kontinue(k, x);

  C_do_register_finalizer(x, proc);
  C_kontinue(k, x);
}


/*XXX could this be made static? is it used in eggs somewhere? 
  if not, declare as fcall/regparm (and static, remove from chicken.h)
 */
void C_ccall C_do_register_finalizer(C_word x, C_word proc)
{
  C_word *ptr;
  int n, i;
  FINALIZER_NODE *flist;

  if(finalizer_free_list == NULL) {
    if((flist = (FINALIZER_NODE *)C_malloc(sizeof(FINALIZER_NODE))) == NULL)
      panic(C_text("out of memory - cannot allocate finalizer node"));

    ++allocated_finalizer_count;
  }
  else {
    flist = finalizer_free_list;
    finalizer_free_list = flist->next;
  }

  if(finalizer_list != NULL) finalizer_list->previous = flist;

  flist->previous = NULL;
  flist->next = finalizer_list;
  finalizer_list = flist;

  if(C_in_stackp(x)) C_mutate_slot(&flist->item, x);
  else flist->item = x;

  if(C_in_stackp(proc)) C_mutate_slot(&flist->finalizer, proc);
  else flist->finalizer = proc;

  ++live_finalizer_count;
}


/*XXX same here */
int C_do_unregister_finalizer(C_word x)
{
  int n;
  FINALIZER_NODE *flist;

  for(flist = finalizer_list; flist != NULL; flist = flist->next) {
    if(flist->item == x) {
      if(flist->previous == NULL) finalizer_list = flist->next;
      else flist->previous->next = flist->next;

      return 1;
    }
  }

  return 0;
}


/* Dynamic loading of shared objects: */

void C_ccall C_set_dlopen_flags(C_word c, C_word *av)
{
  C_word
    /* closure = av[ 0 ] */
    k = av[ 1 ],
    now = av[ 2 ],
    global = av[ 3 ];

#if !defined(NO_DLOAD2) && defined(HAVE_DLFCN_H)
  dlopen_flags = (C_truep(now) ? RTLD_NOW : RTLD_LAZY) | (C_truep(global) ? RTLD_GLOBAL : RTLD_LOCAL);
#endif
  C_kontinue(k, C_SCHEME_UNDEFINED);
}


void C_ccall C_dload(C_word c, C_word *av)
{
  C_word
    /* closure = av[ 0 ] */
    k = av[ 1 ],
    name = av[ 2 ],
    entry = av[ 3 ];

#if !defined(NO_DLOAD2) && (defined(HAVE_DLFCN_H) || defined(HAVE_DL_H) || (defined(HAVE_LOADLIBRARY) && defined(HAVE_GETPROCADDRESS)))
  /* Force minor GC: otherwise the lf may contain pointers to stack-data
     (stack allocated interned symbols, for example) */
  C_save_and_reclaim_args((void *)dload_2, 3, k, name, entry);
#endif

  C_kontinue(k, C_SCHEME_FALSE);
}


#ifdef DLOAD_2_DEFINED
# undef DLOAD_2_DEFINED
#endif

#if !defined(NO_DLOAD2) && defined(HAVE_DL_H) && !defined(DLOAD_2_DEFINED)
# ifdef __hpux__
#  define DLOAD_2_DEFINED
void C_ccall dload_2(C_word c, C_word *av0)
{
  void *handle, *p;
  C_word
    entry = av0[ 0 ],
    name = av0[ 1 ],
    k = av0[ 2 ],,
    av[ 2 ];
  C_char *mname = (C_char *)C_data_pointer(name);

  /*
   * C_fprintf(C_stderr,
   *   "shl_loading %s : %s\n",
   *   (char *) C_data_pointer(name),
   *   (char *) C_data_pointer(entry));
   */

  if ((handle = (void *) shl_load(mname,
				  BIND_IMMEDIATE | DYNAMIC_PATH,
				  0L)) != NULL) {
    shl_t shl_handle = (shl_t) handle;

    /*** This version does not check for C_dynamic_and_unsafe. Fix it. */
    if (shl_findsym(&shl_handle, (char *) C_data_pointer(entry), TYPE_PROCEDURE, &p) == 0) {
      current_module_name = C_strdup(mname);
      current_module_handle = handle;

      if(debug_mode) {
	C_dbg(C_text("debug"), C_text("loading compiled module `%s' (handle is " UWORD_FORMAT_STRING ")\n"),
	      current_module_name, (C_uword)current_module_handle);
      }

      av[ 0 ] = C_SCHEME_UNDEFINED;
      av[ 1 ] = k;
      ((C_proc)p)(2, av);       /* doesn't return */
    } else {
      C_dlerror = (char *) C_strerror(errno);
      shl_unload(shl_handle);
    }
  } else {
    C_dlerror = (char *) C_strerror(errno);
  }

  C_kontinue(k, C_SCHEME_FALSE);
}
# endif
#endif


#if !defined(NO_DLOAD2) && defined(HAVE_DLFCN_H) && !defined(DLOAD_2_DEFINED)
# ifndef __hpux__
#  define DLOAD_2_DEFINED
void C_ccall dload_2(C_word c, C_word *av0)
{
  void *handle, *p, *p2;
  C_word 
    entry = av0[ 0 ],
    name = av0[ 1 ],
    k = av0[ 2 ],
    av[ 2 ];
  C_char *topname = (C_char *)C_data_pointer(entry);
  C_char *mname = (C_char *)C_data_pointer(name);
  C_char *tmp;
  int tmp_len = 0;

  if((handle = C_dlopen(mname, dlopen_flags)) != NULL) {
    if((p = C_dlsym(handle, topname)) == NULL) {
      tmp_len = C_strlen(topname) + 2;
      tmp = (C_char *)C_malloc(tmp_len);

      if(tmp == NULL)
	panic(C_text("out of memory - cannot allocate toplevel name string"));

      C_strlcpy(tmp, C_text("_"), tmp_len);
      C_strlcat(tmp, topname, tmp_len);
      p = C_dlsym(handle, tmp);
      C_free(tmp);
    }

    if(p != NULL) {
      current_module_name = C_strdup(mname);
      current_module_handle = handle;

      if(debug_mode) {
	C_dbg(C_text("debug"), C_text("loading compiled module `%s' (handle is " UWORD_FORMAT_STRING ")\n"),
	      current_module_name, (C_uword)current_module_handle);
      }

      av[ 0 ] = C_SCHEME_UNDEFINED;
      av[ 1 ] = k;
      ((C_proc)p)(2, av); /* doesn't return */
    }

    C_dlclose(handle);
  }
  
  C_dlerror = (char *)dlerror();
  C_kontinue(k, C_SCHEME_FALSE);
}
# endif
#endif


#if !defined(NO_DLOAD2) && (defined(HAVE_LOADLIBRARY) && defined(HAVE_GETPROCADDRESS)) && !defined(DLOAD_2_DEFINED)
# define DLOAD_2_DEFINED
void C_ccall dload_2(C_word c, C_word *av0)
{
  HINSTANCE handle;
  FARPROC p = NULL, p2;
  C_word
    entry = av0[ 0 ],
    name = av0[ 1 ],
    k = av0[ 2 ],
    av[ 2 ];
  C_char *topname = (C_char *)C_data_pointer(entry);
  C_char *mname = (C_char *)C_data_pointer(name);

  /* cannot use LoadLibrary on non-DLLs, so we use extension checking */
  if (C_header_size(name) >= 5) {
    char *n = (char*) C_data_pointer(name);
    int l = C_header_size(name);
    if (C_strncasecmp(".dll", n+l-5, 4) && 
	C_strncasecmp(".so", n+l-4, 3))
      C_kontinue(k, C_SCHEME_FALSE);
  }

  if((handle = LoadLibrary(mname)) != NULL) {
    if ((p = GetProcAddress(handle, topname)) != NULL) {
      current_module_name = C_strdup(mname);
      current_module_handle = handle;

      if(debug_mode) {
	C_dbg(C_text("debug"), C_text("loading compiled module `%s' (handle is " UWORD_FORMAT_STRING ")\n"),
	      current_module_name, (C_uword)current_module_handle);
      }

      av[ 0 ] = C_SCHEME_UNDEFINED;
      av[ 1 ] = k;
      ((C_proc)p)(2, av);       /* doesn't return */
    }
    else FreeLibrary(handle);
  }

  C_dlerror = (char *) C_strerror(errno);
  C_kontinue(k, C_SCHEME_FALSE);
}
#endif


void C_ccall C_become(C_word c, C_word *av) 
{
  C_word
    /* closure = av[ 0 ] */
    k = av[ 1 ],
    table = av[ 2 ],
    tp, x, old, neu, i, *p;

  i = forwarding_table_size;
  p = forwarding_table;

  for(tp = table; tp != C_SCHEME_END_OF_LIST; tp = C_u_i_cdr(tp)) {
    x = C_u_i_car(tp);
    old = C_u_i_car(x);
    neu = C_u_i_cdr(x);

    if(i == 0) {
      if((forwarding_table = (C_word *)realloc(forwarding_table, (forwarding_table_size + 1) * 4 * sizeof(C_word))) == NULL)
	panic(C_text("out of memory - cannot re-allocate forwarding table"));
	
      i = forwarding_table_size;
      p = forwarding_table + forwarding_table_size * 2;
      forwarding_table_size *= 2;
    }

    *(p++) = old;
    *(p++) = neu;
    --i;
  }

  *p = 0;
  C_fromspace_top = C_fromspace_limit;
  C_save_and_reclaim_args((void *)become_2, 1, k);
}


void C_ccall become_2(C_word c, C_word *av)
{
  C_word k = av[ 0 ];

  *forwarding_table = 0;
  C_kontinue(k, C_SCHEME_UNDEFINED);
}


C_regparm C_word C_fcall
C_a_i_cpu_time(C_word **a, int c, C_word buf)
{
  C_word u, s = C_fix(0);

#if defined(C_NONUNIX) || defined(__CYGWIN__)
  if(CLOCKS_PER_SEC == 1000) u = clock();
  else u = C_number(a, C_floor(((double)clock() / (double)CLOCKS_PER_SEC) * 1000));
#else
  struct rusage ru;

  if(C_getrusage(RUSAGE_SELF, &ru) == -1) u = 0;
  else {
    u = C_number(a, C_floor((double)ru.ru_utime.tv_sec * 1000 + ru.ru_utime.tv_usec / 1000));
    s = C_number(a, C_floor((double)ru.ru_stime.tv_sec * 1000 + ru.ru_stime.tv_usec / 1000));
  }
#endif

  /* buf must not be in nursery */
  C_set_block_item(buf, 0, u);
  C_set_block_item(buf, 1, s);
  return buf;
}


C_regparm C_word C_fcall C_a_i_make_locative(C_word **a, int c, C_word type, C_word object, C_word index, C_word weak)
{
  C_word *loc = *a;
  int offset, i, in = C_unfix(index);
  *a = loc + C_SIZEOF_LOCATIVE;

  loc[ 0 ] = C_LOCATIVE_TAG;

  switch(C_unfix(type)) {
  case C_SLOT_LOCATIVE: in *= sizeof(C_word); break;
  case C_U16_LOCATIVE:
  case C_S16_LOCATIVE: in *= 2; break;
  case C_U32_LOCATIVE:
  case C_F32_LOCATIVE:
  case C_S32_LOCATIVE: in *= 4; break;
  case C_F64_LOCATIVE: in *= 8; break;
  }

  offset = in + sizeof(C_header);
  loc[ 1 ] = object + offset;
  loc[ 2 ] = C_fix(offset);
  loc[ 3 ] = type;
  loc[ 4 ] = C_truep(weak) ? C_SCHEME_FALSE : object;

  for(i = 0; i < locative_table_count; ++i)
    if(locative_table[ i ] == C_SCHEME_UNDEFINED) {
      locative_table[ i ] = (C_word)loc;
      return (C_word)loc;
    }

  if(locative_table_count >= locative_table_size) {
    if(debug_mode == 2)
      C_dbg(C_text("debug"), C_text("resizing locative table from %d to %d (count is %d)\n"), 
	    locative_table_size, locative_table_size * 2, locative_table_count);

    locative_table = (C_word *)C_realloc(locative_table, locative_table_size * 2 * sizeof(C_word));

    if(locative_table == NULL) 
      panic(C_text("out of memory - cannot resize locative table"));

    locative_table_size *= 2;
  }

  locative_table[ locative_table_count++ ] = (C_word)loc;
  return (C_word)loc;
}

/* DEPRECATED */
void C_ccall C_locative_ref(C_word c, C_word *av)
{
  C_word
    /* closure = av[ 0 ] */
    k = av[ 1 ],
    loc,
    *ptr, val,
    ab[WORDS_PER_FLONUM], *a = ab;

  if(c != 3) C_bad_argc(c, 3);

  loc = av[ 2 ];

  if(C_immediatep(loc) || C_block_header(loc) != C_LOCATIVE_TAG)
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "locative-ref", loc);

  ptr = (C_word *)C_block_item(loc, 0);

  if(ptr == NULL) barf(C_LOST_LOCATIVE_ERROR, "locative-ref", loc);

  switch(C_unfix(C_block_item(loc, 2))) {
  case C_SLOT_LOCATIVE: C_kontinue(k, *ptr);
  case C_CHAR_LOCATIVE: C_kontinue(k, C_make_character(*((char *)ptr)));
  case C_U8_LOCATIVE: C_kontinue(k, C_fix(*((unsigned char *)ptr)));
  case C_S8_LOCATIVE: C_kontinue(k, C_fix(*((char *)ptr)));
  case C_U16_LOCATIVE: C_kontinue(k, C_fix(*((unsigned short *)ptr)));
  case C_S16_LOCATIVE: C_kontinue(k, C_fix(*((short *)ptr)));
  case C_U32_LOCATIVE: C_kontinue(k, C_unsigned_int_to_num(&a, *((C_u32 *)ptr)));
  case C_S32_LOCATIVE: C_kontinue(k, C_int_to_num(&a, *((C_s32 *)ptr)));
  case C_F32_LOCATIVE: C_kontinue(k, C_flonum(&a, *((float *)ptr)));
  case C_F64_LOCATIVE: C_kontinue(k, C_flonum(&a, *((double *)ptr)));
  default: panic(C_text("bad locative type"));
  }
}

C_regparm C_word C_fcall C_a_i_locative_ref(C_word **a, int c, C_word loc)
{
  C_word *ptr;

  if(C_immediatep(loc) || C_block_header(loc) != C_LOCATIVE_TAG)
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "locative-ref", loc);

  ptr = (C_word *)C_block_item(loc, 0);

  if(ptr == NULL) barf(C_LOST_LOCATIVE_ERROR, "locative-ref", loc);

  switch(C_unfix(C_block_item(loc, 2))) {
  case C_SLOT_LOCATIVE: return *ptr;
  case C_CHAR_LOCATIVE: return C_make_character(*((char *)ptr));
  case C_U8_LOCATIVE: return C_fix(*((unsigned char *)ptr));
  case C_S8_LOCATIVE: return C_fix(*((char *)ptr));
  case C_U16_LOCATIVE: return C_fix(*((unsigned short *)ptr));
  case C_S16_LOCATIVE: return C_fix(*((short *)ptr));
  case C_U32_LOCATIVE: return C_unsigned_int_to_num(a, *((C_u32 *)ptr));
  case C_S32_LOCATIVE: return C_int_to_num(a, *((C_s32 *)ptr));
  case C_F32_LOCATIVE: return C_flonum(a, *((float *)ptr));
  case C_F64_LOCATIVE: return C_flonum(a, *((double *)ptr));
  default: panic(C_text("bad locative type"));
  }
}

C_regparm C_word C_fcall C_i_locative_set(C_word loc, C_word x)
{
  C_word *ptr, val;

  if(C_immediatep(loc) || C_block_header(loc) != C_LOCATIVE_TAG)
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "locative-set!", loc);

  ptr = (C_word *)C_block_item(loc, 0);

  if(ptr == NULL)
    barf(C_LOST_LOCATIVE_ERROR, "locative-set!", loc);

  switch(C_unfix(C_block_item(loc, 2))) {
  case C_SLOT_LOCATIVE: C_mutate2(ptr, x); break;

  case C_CHAR_LOCATIVE:
    if((x & C_IMMEDIATE_TYPE_BITS) != C_CHARACTER_BITS)
      barf(C_BAD_ARGUMENT_TYPE_ERROR, "locative-set!", x);
      
    *((char *)ptr) = C_character_code(x); 
    break;

  case C_U8_LOCATIVE: 
    if((x & C_FIXNUM_BIT) == 0)
      barf(C_BAD_ARGUMENT_TYPE_ERROR, "locative-set!", x);

    *((unsigned char *)ptr) = C_unfix(x); 
    break;

  case C_S8_LOCATIVE: 
    if((x & C_FIXNUM_BIT) == 0)
      barf(C_BAD_ARGUMENT_TYPE_ERROR, "locative-set!", x);
    
    *((char *)ptr) = C_unfix(x); 
    break;

  case C_U16_LOCATIVE: 
    if((x & C_FIXNUM_BIT) == 0)
      barf(C_BAD_ARGUMENT_TYPE_ERROR, "locative-set!", x);

    *((unsigned short *)ptr) = C_unfix(x); 
    break;

  case C_S16_LOCATIVE: 
    if((x & C_FIXNUM_BIT) == 0)
      barf(C_BAD_ARGUMENT_TYPE_ERROR, "locative-set!", x);

    *((short *)ptr) = C_unfix(x); 
    break;

  case C_U32_LOCATIVE: 
    if((x & C_FIXNUM_BIT) == 0 && (C_immediatep(x) || C_block_header(x) != C_FLONUM_TAG))
      barf(C_BAD_ARGUMENT_TYPE_ERROR, "locative-set!", x);

    *((C_u32 *)ptr) = C_num_to_unsigned_int(x); 
    break;

  case C_S32_LOCATIVE: 
    if((x & C_FIXNUM_BIT) == 0 && (C_immediatep(x) || C_block_header(x) != C_FLONUM_TAG))
      barf(C_BAD_ARGUMENT_TYPE_ERROR, "locative-set!", x);
    
    *((C_s32 *)ptr) = C_num_to_int(x); 
    break;

  case C_F32_LOCATIVE: 
    if(C_immediatep(x) || C_block_header(x) != C_FLONUM_TAG)
      barf(C_BAD_ARGUMENT_TYPE_ERROR, "locative-set!", x);
    
    *((float *)ptr) = C_flonum_magnitude(x); 
    break;

  case C_F64_LOCATIVE: 
    if(C_immediatep(x) || C_block_header(x) != C_FLONUM_TAG)
      barf(C_BAD_ARGUMENT_TYPE_ERROR, "locative-set!", x);
    
    *((double *)ptr) = C_flonum_magnitude(x); 
    break;

  default: panic(C_text("bad locative type"));
  }

  return C_SCHEME_UNDEFINED;
}


C_regparm C_word C_fcall C_i_locative_to_object(C_word loc)
{
  C_word *ptr;

  if(C_immediatep(loc) || C_block_header(loc) != C_LOCATIVE_TAG)
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "locative->object", loc);

  ptr = (C_word *)C_block_item(loc, 0);

  if(ptr == NULL) return C_SCHEME_FALSE;
  else return (C_word)ptr - C_unfix(C_block_item(loc, 1));
}


/* GC protection of user-variables: */

C_regparm void C_fcall C_gc_protect(C_word **addr, int n)
{
  int k;

  if(collectibles_top + n >= collectibles_limit) {
    k = collectibles_limit - collectibles;
    collectibles = (C_word **)C_realloc(collectibles, sizeof(C_word *) * k * 2);

    if(collectibles == NULL)
      panic(C_text("out of memory - cannot allocate GC protection vector"));
    
    collectibles_top = collectibles + k;
    collectibles_limit = collectibles + k * 2;
  }

  C_memcpy(collectibles_top, addr, n * sizeof(C_word *));
  collectibles_top += n;
}


C_regparm void C_fcall C_gc_unprotect(int n)
{
  collectibles_top -= n;
}


/* Map procedure-ptr to id or id to ptr: */

C_char *C_lookup_procedure_id(void *ptr)
{
  LF_LIST *lfl;
  C_PTABLE_ENTRY *pt;

  for(lfl = lf_list; lfl != NULL; lfl = lfl->next) {
    pt = lfl->ptable;

    if(pt != NULL) {
      while(pt->id != NULL) {
	if(pt->ptr == ptr) return pt->id;
	else ++pt;
      }
    }
  }

  return NULL;
}


void *C_lookup_procedure_ptr(C_char *id)
{
  LF_LIST *lfl;
  C_PTABLE_ENTRY *pt;

  for(lfl = lf_list; lfl != NULL; lfl = lfl->next) {
    pt = lfl->ptable;

    if(pt != NULL) {
      while(pt->id != NULL) {
	if(!C_strcmp(id, pt->id)) return pt->ptr;
	else ++pt;
      }
    }
  }

  return NULL;
}


void C_ccall C_copy_closure(C_word c, C_word *av)
{
  C_word
    /* closure = av[ 0 ] */
    k = av[ 1 ],
    proc = av[ 2 ],
    *p;
  int n = C_header_size(proc);

  if(!C_demand(n + 1)) 
    C_save_and_reclaim_args((void *)copy_closure_2, 2, proc, k);
  else {
    C_save(proc);
    C_save(k);
    p = C_temporary_stack;
    C_temporary_stack = C_temporary_stack_bottom;
    copy_closure_2(0, p);
  }
}


static void C_ccall copy_closure_2(C_word c, C_word *av)
{
  C_word 
    k = av[ 0 ],
    proc = av[ 1 ];
  int cells = C_header_size(proc);
  C_word
    *ptr = C_alloc(cells + 1),
    *p = ptr;

  *(p++) = C_CLOSURE_TYPE | cells;
  /* this is only allowed because the storage is freshly allocated: */
  C_memcpy_slots(p, C_data_pointer(proc), cells);
  C_kontinue(k, (C_word)ptr);
}


/* Ph'nglui mglw'nafh Cthulhu R'lyeh wgah'nagl fhtagn */

void C_ccall C_call_with_cthulhu(C_word c, C_word *av)
{
  C_word
    proc = av[ 2 ],
    *a = C_alloc(3),
    av2[ 2 ];

  av2[ 0 ] = proc;
  av2[ 1 ] = C_closure(&a, 1, (C_word)termination_continuation); /* k */
  C_do_apply(2, av2);
}


/* fixnum arithmetic with overflow detection (from "Hacker's Delight" by Hank Warren) 
   These routines return #f if the operation failed due to overflow. 
 */

C_regparm C_word C_fcall C_i_o_fixnum_plus(C_word n1, C_word n2)
{
  C_word x1, x2, s;
  
  if((n1 & C_FIXNUM_BIT) == 0 || (n2 & C_FIXNUM_BIT) == 0) return C_SCHEME_FALSE;

  x1 = C_unfix(n1);
  x2 = C_unfix(n2);
  s = x1 + x2;

#ifdef C_SIXTY_FOUR
  if((((s ^ x1) & (s ^ x2)) >> 62) != 0) return C_SCHEME_FALSE;
#else
  if((((s ^ x1) & (s ^ x2)) >> 30) != 0) return C_SCHEME_FALSE;
#endif
  else return C_fix(s);
}


C_regparm C_word C_fcall C_i_o_fixnum_difference(C_word n1, C_word n2)
{
  C_word x1, x2, s;

  if((n1 & C_FIXNUM_BIT) == 0 || (n2 & C_FIXNUM_BIT) == 0) return C_SCHEME_FALSE;

  x1 = C_unfix(n1);
  x2 = C_unfix(n2);
  s = x1 - x2;
  
#ifdef C_SIXTY_FOUR
  if((((s ^ x1) & ~(s ^ x2)) >> 62) != 0) return C_SCHEME_FALSE;
#else
  if((((s ^ x1) & ~(s ^ x2)) >> 30) != 0) return C_SCHEME_FALSE;
#endif
  else return C_fix(s);
}


C_regparm C_word C_fcall C_i_o_fixnum_times(C_word n1, C_word n2)
{
  C_word x1, x2;
  C_uword x1u, x2u;
#ifdef C_SIXTY_FOUR
# ifdef C_LLP
  C_uword c = 1ULL<<63ULL;
# else
  C_uword c = 1UL<<63UL;
# endif
#else
  C_uword c = 1UL<<31UL;
#endif

  if((n1 & C_FIXNUM_BIT) == 0 || (n2 & C_FIXNUM_BIT) == 0) return C_SCHEME_FALSE;

  if((n1 & C_INT_SIGN_BIT) == (n2 & C_INT_SIGN_BIT)) --c;

  x1 = C_unfix(n1);
  x2 = C_unfix(n2);
  x1u = x1 < 0 ? -x1 : x1;
  x2u = x2 < 0 ? -x2 : x2;

  if(x2u != 0 && x1u > (c / x2u)) return C_SCHEME_FALSE;
  
  x1 = x1 * x2;

  if(C_fitsinfixnump(x1)) return C_fix(x1);
  else return C_SCHEME_FALSE;
}


C_regparm C_word C_fcall C_i_o_fixnum_quotient(C_word n1, C_word n2)
{
  C_word x1, x2;

  if((n1 & C_FIXNUM_BIT) == 0 || (n2 & C_FIXNUM_BIT) == 0) return C_SCHEME_FALSE;

  x1 = C_unfix(n1);
  x2 = C_unfix(n2);

  if(x2 == 0)
    barf(C_DIVISION_BY_ZERO_ERROR, "fx/?");

#ifdef C_SIXTY_FOUR
  if(x1 == 0x8000000000000000L && x2 == -1) return C_SCHEME_FALSE;
#else
  if(x1 == 0x80000000L && x2 == -1) return C_SCHEME_FALSE;
#endif

  x1 = x1 / x2;

  if(C_fitsinfixnump(x1)) return C_fix(x1);
  else return C_SCHEME_FALSE;
}


C_regparm C_word C_fcall C_i_o_fixnum_and(C_word n1, C_word n2)
{
  C_uword x1, x2, r;

  if((n1 & C_FIXNUM_BIT) == 0 || (n2 & C_FIXNUM_BIT) == 0) return C_SCHEME_FALSE;

  x1 = C_unfix(n1);
  x2 = C_unfix(n2);
  r = x1 & x2;
  
  if(((r & C_INT_SIGN_BIT) >> 1) != (r & C_INT_TOP_BIT)) return C_SCHEME_FALSE;
  else return C_fix(r);
}


C_regparm C_word C_fcall C_i_o_fixnum_ior(C_word n1, C_word n2)
{
  C_uword x1, x2, r;

  if((n1 & C_FIXNUM_BIT) == 0 || (n2 & C_FIXNUM_BIT) == 0) return C_SCHEME_FALSE;

  x1 = C_unfix(n1);
  x2 = C_unfix(n2);
  r = x1 | x2;
  
  if(((r & C_INT_SIGN_BIT) >> 1) != (r & C_INT_TOP_BIT)) return C_SCHEME_FALSE;
  else return C_fix(r);
}


C_regparm C_word C_fcall C_i_o_fixnum_xor(C_word n1, C_word n2)
{
  C_uword x1, x2, r;

  if((n1 & C_FIXNUM_BIT) == 0 || (n2 & C_FIXNUM_BIT) == 0) return C_SCHEME_FALSE;

  x1 = C_unfix(n1);
  x2 = C_unfix(n2);
  r = x1 ^ x2;
  
  if(((r & C_INT_SIGN_BIT) >> 1) != (r & C_INT_TOP_BIT)) return C_SCHEME_FALSE;
  else return C_fix(r);
}


/* decoding of literals in compressed format */

static C_regparm C_uword C_fcall decode_size(C_char **str)
{
  C_uchar **ustr = (C_uchar **)str;
  C_uword size = (*((*ustr)++) & 0xff) << 16; /* always big endian */

  size |= (*((*ustr)++) & 0xff) << 8;
  size |= (*((*ustr)++) & 0xff);
  return size;
}


static C_regparm C_word C_fcall decode_literal2(C_word **ptr, C_char **str,
						C_word *dest)
{
  C_ulong bits = *((*str)++) & 0xff;
  C_word *data, *dptr, val;
  C_uword size;

  /* vvv this can be taken out at a later stage (once it works reliably) vvv */
  if(bits != 0xfe)
    panic(C_text("invalid encoded literal format"));

  bits = *((*str)++) & 0xff;
  /* ^^^ */

#ifdef C_SIXTY_FOUR
  bits <<= 24 + 32;
#else
  bits <<= 24;
#endif

  if(bits == C_HEADER_BITS_MASK) {		/* special/immediate */
    switch(0xff & *((*str)++)) {
    case C_BOOLEAN_BITS: 
      return C_mk_bool(*((*str)++));

    case C_CHARACTER_BITS: 
      return C_make_character(decode_size(str));

    case C_SCHEME_END_OF_LIST:
    case C_SCHEME_UNDEFINED:
    case C_SCHEME_END_OF_FILE:
      return (C_word)(*(*str - 1));

    case C_FIXNUM_BIT:
      val = (C_uword)(signed char)*((*str)++) << 24; /* always big endian */
      val |= ((C_uword)*((*str)++) & 0xff) << 16;
      val |= ((C_uword)*((*str)++) & 0xff) << 8;
      val |= ((C_uword)*((*str)++) & 0xff);
      return C_fix(val); 

#ifdef C_SIXTY_FOUR
    case (C_FLONUM_TYPE >> (24 + 32)) & 0xff:
#else
    case (C_FLONUM_TYPE >> 24) & 0xff:
#endif
      bits = C_FLONUM_TYPE;
      break;

    default: 
      panic(C_text("invalid encoded special literal"));
    }
  }

#ifndef C_SIXTY_FOUR
  if((bits & C_8ALIGN_BIT) != 0) {
    /* Align _data_ on 8-byte boundary: */
    if(C_aligned8(*ptr)) ++(*ptr);
  }
#endif

  val = (C_word)(*ptr);

  if(bits == C_FLONUM_TYPE) {
    C_word ln;
    double fn;

    switch (convert_string_to_number(*str, 10, &ln, &fn)) {
    case 0: 			/* failed */
      panic(C_text("invalid encoded numeric literal"));
      break;

    case 1:			/* fixnum */
      val = C_fix(ln);
      break;

    case 2:			/* flonum */
      val = C_flonum(ptr, fn);
      break;
    }

    while(*((*str)++) != '\0');      /* skip terminating '\0' */
    return val;
  }

  if((bits & C_SPECIALBLOCK_BIT) != 0)
    panic(C_text("literals with special bit cannot be decoded"));

  size = decode_size(str);

  switch(bits) {
  case C_STRING_TYPE:
    /* strings are always allocated statically */
    val = C_static_string(ptr, size, *str);
    *str += size;
    break;
    
  case C_BYTEVECTOR_TYPE:
    /* ... as are bytevectors (blobs) */
    val = C_static_bytevector(ptr, size, *str);
    *str += size;
    break;
    
  case C_SYMBOL_TYPE:
    if(dest == NULL) 
      panic(C_text("invalid literal symbol destination"));

    val = C_h_intern(dest, size, *str);
    *str += size;
    break;

  case C_LAMBDA_INFO_TYPE:
    /* lambda infos are always allocated statically */
    val = C_static_lambda_info(ptr, size, *str);
    *str += size;
    break;

  default:
    *((*ptr)++) = C_make_header(bits, size);
    data = *ptr;

    if((bits & C_BYTEBLOCK_BIT) != 0) {
      C_memcpy(data, *str, size);
      size = C_align(size);
      *str += size;
      *ptr = (C_word *)C_align((C_word)(*ptr) + size);
    }
    else {
      C_word *dptr = *ptr;
      *ptr += size;

      while(size--) {
	*dptr = decode_literal2(ptr, str, dptr);
	++dptr;
      }
    }
  }

  return val;
}


C_regparm C_word C_fcall
C_decode_literal(C_word **ptr, C_char *str)
{
  return decode_literal2(ptr, &str, NULL);
}


void
C_use_private_repository(C_char *path)
{
  private_repository = path == NULL ? NULL : C_strdup(path);
}


C_char *
C_private_repository_path()
{
  return private_repository;
}


C_regparm C_word C_fcall
C_i_getprop(C_word sym, C_word prop, C_word def)
{
  C_word pl = C_block_item(sym, 2);

  while(pl != C_SCHEME_END_OF_LIST) {
    if(C_block_item(pl, 0) == prop)
      return C_u_i_car(C_u_i_cdr(pl));
    else pl = C_u_i_cdr(C_u_i_cdr(pl));
  }

  return def;
}


C_regparm C_word C_fcall
C_putprop(C_word **ptr, C_word sym, C_word prop, C_word val)
{
  C_word pl = C_block_item(sym, 2);

  while(pl != C_SCHEME_END_OF_LIST) {
    if(C_block_item(pl, 0) == prop) {
      C_mutate2(&C_u_i_car(C_u_i_cdr(pl)), val);
      return val;
    }
    else pl = C_u_i_cdr(C_u_i_cdr(pl));
  }

  pl = C_a_pair(ptr, val, C_block_item(sym, 2));
  pl = C_a_pair(ptr, prop, pl);
  C_mutate_slot(&C_block_item(sym, 2), pl);
  return val;
}


C_regparm C_word C_fcall
C_i_get_keyword(C_word kw, C_word args, C_word def)
{
  while(!C_immediatep(args)) {
    if(C_block_header(args) == C_PAIR_TAG) {
      if(kw == C_u_i_car(args)) {
	args = C_u_i_cdr(args);

	if(C_immediatep(args) || C_block_header(args) != C_PAIR_TAG)
	  return def;
	else return C_u_i_car(args);
      }
      else {
	args = C_u_i_cdr(args);

	if(C_immediatep(args) || C_block_header(args) != C_PAIR_TAG)
	  return def;
	else args = C_u_i_cdr(args);
      }
    }
  }

  return def;
}

C_word C_i_dump_statistical_profile()
{
  PROFILE_BUCKET *b, *b2, **bp;
  FILE *fp;
  C_char *k1, *k2 = NULL;
  int n;
  double ms;

  assert(profiling);
  assert(profile_table != NULL);

  set_profile_timer(0);

  profiling = 0; /* In case a SIGPROF is delivered late */
  bp = profile_table;

  C_snprintf(buffer, STRING_BUFFER_SIZE, C_text("PROFILE.%d"), C_getpid());

  if(debug_mode)
    C_dbg(C_text("debug"), C_text("dumping statistical profile to `%s'...\n"), buffer);

  fp = C_fopen(buffer, "w");
  if (fp == NULL)
    panic(C_text("could not write profile!"));

  C_fputs(C_text("statistical\n"), fp);
  for(n = 0; n < PROFILE_TABLE_SIZE; ++n) {
    for(b = bp[ n ]; b != NULL; b = b2) {
      b2 = b->next;

      k1 = b->key;
      C_fputs(C_text("(|"), fp);
      /* Dump raw C string as if it were a symbol */
      while((k2 = C_strpbrk(k1, C_text("\\|"))) != NULL) {
        C_fwrite(k1, 1, k2-k1, fp);
        C_fputc('\\', fp);
        C_fputc(*k2, fp);
        k1 = k2+1;
      }
      C_fputs(k1, fp);
      ms = (double)b->sample_count * (double)profile_frequency / 1000.0;
      C_fprintf(fp, C_text("| " UWORD_COUNT_FORMAT_STRING " %lf)\n"),
                b->call_count, ms);
      C_free(b);
    }
  }

  C_fclose(fp);
  C_free(profile_table);
  profile_table = NULL;

  return C_SCHEME_UNDEFINED;
}

void C_ccall C_dump_heap_state(C_word c, C_word *av)
{
  C_word
    /* closure = av[ 0 ] */
    k = av[ 1 ];

  /* make sure heap is compacted */
  C_save(k);
  C_fromspace_top = C_fromspace_limit; /* force major GC */
  C_reclaim((void *)dump_heap_state_2, 1);
}


static C_ulong
hdump_hash(C_word key)
{
  return (C_ulong)key % HDUMP_TABLE_SIZE;
}


static void
hdump_count(C_word key, int n, int t)
{
  HDUMP_BUCKET **bp = hdump_table + hdump_hash(key);
  HDUMP_BUCKET *b = *bp;

  while(b != NULL) {
    if(b->key == key) {
      b->count += n;
      b->total += t;
      return;
    }
    else b = b->next;
  }

  b = (HDUMP_BUCKET *)C_malloc(sizeof(HDUMP_BUCKET));
  
  if(b == 0)
    panic(C_text("out of memory - can not allocate heap-dump table-bucket"));

  b->next = *bp;
  b->key = key;
  *bp = b;
  b->count = n;
  b->total = t;
}


static void C_ccall dump_heap_state_2(C_word c, C_word *av)
{
  C_word k = av[ 0 ];
  HDUMP_BUCKET *b, *b2, **bp;
  int n, bytes;
  C_byte *scan;
  C_SCHEME_BLOCK *sbp;
  C_header h;
  C_word x, key, *p;
  int imm = 0, blk = 0;
  
  hdump_table = (HDUMP_BUCKET **)C_malloc(HDUMP_TABLE_SIZE * sizeof(HDUMP_BUCKET *));

  if(hdump_table == NULL)
    panic(C_text("out of memory - can not allocate heap-dump table"));

  C_memset(hdump_table, 0, sizeof(HDUMP_BUCKET *) * HDUMP_TABLE_SIZE);

  scan = fromspace_start;

  while(scan < C_fromspace_top) {
    ++blk;
    sbp = (C_SCHEME_BLOCK *)scan;

    if(*((C_word *)sbp) == ALIGNMENT_HOLE_MARKER) 
      sbp = (C_SCHEME_BLOCK *)((C_word *)sbp + 1);

    n = C_header_size(sbp);
    h = sbp->header;
    bytes = (h & C_BYTEBLOCK_BIT) ? n : n * sizeof(C_word);
    key = (C_word)(h & C_HEADER_BITS_MASK);
    p = sbp->data;

    if(key == C_STRUCTURE_TYPE) key = *p;

    hdump_count(key, 1, bytes);

    if(n > 0 && (h & C_BYTEBLOCK_BIT) == 0) {
      if((h & C_SPECIALBLOCK_BIT) != 0) {
	--n;
	++p;
      }

      while(n--) {
	x = *(p++);

	if(C_immediatep(x)) {
	  ++imm;

	  if((x & C_FIXNUM_BIT) != 0) key = C_fix(1);
	  else {
	    switch(x & C_IMMEDIATE_TYPE_BITS) {
	    case C_BOOLEAN_BITS: key = C_SCHEME_TRUE; break;
	    case C_CHARACTER_BITS: key = C_make_character('A'); break;
	    default: key = x;
	    }
	  }

	  hdump_count(key, 1, 0);
	}
      }
    }

    scan = (C_byte *)sbp + C_align(bytes) + sizeof(C_word);
  }

  bp = hdump_table;
  
  for(n = 0; n < HDUMP_TABLE_SIZE; ++n) {
    for(b = bp[ n ]; b != NULL; b = b2) {
      b2 = b->next;

      switch(b->key) {
      case C_fix(1): C_fprintf(C_stderr,              C_text("fixnum         ")); break;
      case C_SCHEME_TRUE: C_fprintf(C_stderr,         C_text("boolean        ")); break;
      case C_SCHEME_END_OF_LIST: C_fprintf(C_stderr,  C_text("null           ")); break;
      case C_SCHEME_UNDEFINED  : C_fprintf(C_stderr,  C_text("void           ")); break;
      case C_make_character('A'): C_fprintf(C_stderr, C_text("character      ")); break;
      case C_SCHEME_END_OF_FILE: C_fprintf(C_stderr,  C_text("eof            ")); break;
      case C_SCHEME_UNBOUND: C_fprintf(C_stderr,      C_text("unbound        ")); break;
      case C_SYMBOL_TYPE: C_fprintf(C_stderr,         C_text("symbol         ")); break;
      case C_STRING_TYPE: C_fprintf(C_stderr,         C_text("string         ")); break;
      case C_PAIR_TYPE: C_fprintf(C_stderr,           C_text("pair           ")); break;
      case C_CLOSURE_TYPE: C_fprintf(C_stderr,        C_text("closure        ")); break;
      case C_FLONUM_TYPE: C_fprintf(C_stderr,         C_text("flonum         ")); break;
      case C_PORT_TYPE: C_fprintf(C_stderr,           C_text("port           ")); break;
      case C_POINTER_TYPE: C_fprintf(C_stderr,        C_text("pointer        ")); break;
      case C_LOCATIVE_TYPE: C_fprintf(C_stderr,       C_text("locative       ")); break;
      case C_TAGGED_POINTER_TYPE: C_fprintf(C_stderr, C_text("tagged pointer ")); break;
      case C_SWIG_POINTER_TYPE: C_fprintf(C_stderr,   C_text("swig pointer   ")); break;
      case C_LAMBDA_INFO_TYPE: C_fprintf(C_stderr,    C_text("lambda info    ")); break;
      case C_BUCKET_TYPE: C_fprintf(C_stderr,         C_text("bucket         ")); break;
      case C_VECTOR_TYPE: C_fprintf(C_stderr,         C_text("vector         ")); break;
      case C_BYTEVECTOR_TYPE: C_fprintf(C_stderr,     C_text("bytevector     ")); break;
	/* XXX this is sort of funny: */
      case C_BYTEBLOCK_BIT: C_fprintf(C_stderr,        C_text("blob           ")); break;
      default:
	x = b->key;

	if(!C_immediatep(x) && C_header_bits(x) == C_SYMBOL_TYPE) {
	  x = C_block_item(x, 1);
	  C_fprintf(C_stderr, C_text("`%.*s'"), (int)C_header_size(x), C_c_string(x));
	}
	else C_fprintf(C_stderr, C_text("unknown key " UWORD_FORMAT_STRING), (C_uword)b->key);
      }

      C_fprintf(C_stderr, C_text("\t%d"), b->count);

      if(b->total > 0) 
	C_fprintf(C_stderr, C_text("\t%d bytes"), b->total);

      C_fputc('\n', C_stderr);
      C_free(b);
    }
  }

  C_fprintf(C_stderr, C_text("\ntotal number of blocks: %d, immediates: %d\n"),
	    blk, imm);
  C_free(hdump_table);
  C_kontinue(k, C_SCHEME_UNDEFINED);
}


static void C_ccall filter_heap_objects_2(C_word c, C_word *av)
{
  void *func = C_pointer_address(av[ 0 ]);
  C_word 
    userarg = av[ 1 ],
    vector = av[ 2 ],
    k = av[ 3 ];
  int n, bytes;
  C_byte *scan;
  C_SCHEME_BLOCK *sbp;
  C_header h;
  C_word *p;
  int vecsize = C_header_size(vector);
  typedef int (*filterfunc)(C_word x, C_word userarg);
  filterfunc ff = (filterfunc)func;
  int vcount = 0;

  scan = fromspace_start;

  while(scan < C_fromspace_top) {
    sbp = (C_SCHEME_BLOCK *)scan;

    if(*((C_word *)sbp) == ALIGNMENT_HOLE_MARKER) 
      sbp = (C_SCHEME_BLOCK *)((C_word *)sbp + 1);

    n = C_header_size(sbp);
    h = sbp->header;
    bytes = (h & C_BYTEBLOCK_BIT) ? n : n * sizeof(C_word);
    p = sbp->data;

    if(ff((C_word)sbp, userarg)) {
      if(vcount < vecsize) {
	C_set_block_item(vector, vcount, (C_word)sbp);
	++vcount;
      }
      else {
	C_kontinue(k, C_fix(-1));
      }
    }

    scan = (C_byte *)sbp + C_align(bytes) + sizeof(C_word);
  }

  C_kontinue(k, C_fix(vcount));
}


void C_ccall C_filter_heap_objects(C_word c, C_word *av)
{
  C_word
    /* closure = av[ 0 ] */
    k = av[ 1 ],
    func = av[ 2 ],
    vector = av[ 3 ],
    userarg = av[ 4 ];

  /* make sure heap is compacted */
  C_save(k);
  C_save(vector);
  C_save(userarg);
  C_save(func);
  C_fromspace_top = C_fromspace_limit; /* force major GC */
  C_reclaim((void *)filter_heap_objects_2, 4);
}


C_regparm C_word C_fcall 
C_i_file_exists_p(C_word name, C_word file, C_word dir)
{
  struct stat buf;
  int res;

  res = stat(C_c_string(name), &buf);

  if(res != 0) {
    switch(errno) {
    case ENOENT: return C_SCHEME_FALSE;
    case EOVERFLOW: return C_truep(dir) ? C_SCHEME_FALSE : C_SCHEME_TRUE;
    case ENOTDIR: return C_SCHEME_FALSE;
    default: return C_fix(res);
    }
  }

  switch(buf.st_mode & S_IFMT) {
  case S_IFDIR: return C_truep(file) ? C_SCHEME_FALSE : C_SCHEME_TRUE;
  default: return C_truep(dir) ? C_SCHEME_FALSE : C_SCHEME_TRUE;
  }
}


C_regparm C_word C_fcall
C_i_pending_interrupt(C_word dummy)
{
  if(pending_interrupts_count > 0) {
    handling_interrupts = 1; /* Lock out further forced GCs until we're done */
    return C_fix(pending_interrupts[ --pending_interrupts_count ]);
  } else {
    handling_interrupts = 0; /* OK, can go on */
    return C_SCHEME_FALSE;
  }
}
