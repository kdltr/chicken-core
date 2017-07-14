#!/bin/sh
# runtests.sh - run CHICKEN testsuite
#
# - Note: this needs a proper shell, so it will not work with plain mingw
#   (just the compiler and the Windows shell, without MSYS)


set -e
if test -z "$MSYSTEM"; then
    TEST_DIR=`pwd`
else
    # Use Windows-native format with drive letters instead of awkward
    # MSYS /c/blabla "pseudo-paths" which break when used in syscalls.
    TEST_DIR=`pwd -W`
fi

DYLD_LIBRARY_PATH=${TEST_DIR}/..
LD_LIBRARY_PATH=${TEST_DIR}/..
LIBRARY_PATH=${TEST_DIR}/..:${LIBRARY_PATH}
# Cygwin uses LD_LIBRARY_PATH for dlopen(), but the dlls linked into
# the binary are read by the OS itself, which uses $PATH (mingw too)
# Oddly, prefixing .. with ${TEST_DIR}/ does _not_ work on mingw!
PATH=..:${PATH}

export DYLD_LIBRARY_PATH LD_LIBRARY_PATH LIBRARY_PATH PATH

case `uname` in
	AIX)
		DIFF_OPTS=-b ;;
	*)
		DIFF_OPTS=-bu ;;
esac

CHICKEN=${TEST_DIR}/../chicken
CHICKEN_PROFILE=${TEST_DIR}/../chicken-profile
CHICKEN_INSTALL=${TEST_DIR}/../chicken-install
CHICKEN_UNINSTALL=${TEST_DIR}/../chicken-uninstall
CHICKEN_INSTALL_REPOSITORY=${TEST_DIR}/test-repository
CHICKEN_REPOSITORY_PATH=${TEST_DIR}/..:$CHICKEN_INSTALL_REPOSITORY
COMPILE_OPTIONS="-compiler ${CHICKEN} -v -I${TEST_DIR}/.. -L${TEST_DIR}/.. -rpath ${TEST_DIR}/.. -include-path ${TEST_DIR}/.."

export CHICKEN_INSTALL_REPOSITORY CHICKEN_REPOSITORY_PATH

TYPESDB=../types.db

compile="../csc -types ${TYPESDB} -ignore-repository ${COMPILE_OPTIONS} -o a.out"
compile2="../csc -compiler ${CHICKEN} -v -I${TEST_DIR}/.. -L${TEST_DIR}/.. -include-path ${TEST_DIR}/.."
compile_s="../csc -s -types ${TYPESDB} -ignore-repository ${COMPILE_OPTIONS}"
interpret="../csi -n -include-path ${TEST_DIR}/.."
time=time

# Check for a "time" command, since some systems don't ship with a
# time(1) or shell builtin and we also can't portably rely on `which',
# `command', etc. NOTE "time" must be called from a variable here.
set +e
$time true >/dev/null 2>/dev/null
test $? -eq 127 && time=
set -e

rm -fr *.exe *.so *.o *.import.* a.out ../foo.import.* test-repository
mkdir -p test-repository
cp $TYPESDB test-repository/types.db

echo "======================================== version tests ..."
$compile version-tests.scm
./a.out

echo "======================================== compiler tests ..."
$compile compiler-tests.scm
./a.out

echo "======================================== compiler inlining tests  ..."
$compile inlining-tests.scm -optimize-level 3
./a.out

echo "======================================== compiler message tests ..."
$compile -analyze-only messages-test.scm 2>messages.out
diff $DIFF_OPTS messages.expected messages.out

echo "======================================== optimizer tests  ..."
$compile clustering-tests.scm -clustering
./a.out

echo "======================================== profiler tests ..."
$compile null.scm -profile -profile-name TEST.profile
./a.out
$CHICKEN_PROFILE TEST.profile

echo "======================================== scrutiny tests ..."
$compile scrutinizer-tests.scm -analyze-only
$compile typematch-tests.scm -specialize -no-warnings
./a.out

$compile scrutiny-tests.scm -analyze-only -verbose 2>scrutiny.out
$compile specialization-tests.scm -analyze-only -verbose -specialize 2>specialization.out

# these are sensitive to gensym-names, so make them optional
if test \! -f scrutiny.expected; then
    cp scrutiny.out scrutiny.expected
fi
if test \! -f specialization.expected; then
    cp specialization.out specialization.expected
fi

diff $DIFF_OPTS scrutiny.expected scrutiny.out
diff $DIFF_OPTS specialization.expected specialization.out

$compile scrutiny-tests-2.scm -A 2>scrutiny-2.out -verbose

# this is sensitive to gensym-names, so make it optional
if test \! -f scrutiny-2.expected; then
    cp scrutiny-2.out scrutiny-2.expected
fi

diff $DIFF_OPTS scrutiny-2.expected scrutiny-2.out

$compile scrutiny-tests-3.scm -specialize -block
./a.out

$compile scrutiny-tests-strict.scm -strict-types -specialize
./a.out

echo "======================================== specialization tests ..."
rm -f foo.types foo.import.*
$compile specialization-test-1.scm -emit-type-file foo.types -specialize \
  -debug ox -emit-import-library foo
./a.out
$compile specialization-test-2.scm -types foo.types -types specialization-test-2.types -specialize -debug ox
./a.out
rm -f foo.types foo.import.*

echo "======================================== specialization benchmark ..."
$compile fft.scm -O2 -local -d0 -disable-interrupts -b -o fft1
$compile fft.scm -O2 -local -specialize -debug x -d0 -disable-interrupts -b -o fft2 -specialize
echo "normal:"
$time ./fft1 1000 7
echo "specialized:"
$time ./fft2 1000 7

echo "======================================== callback tests ..."
$compile -extend c-id-valid.scm callback-tests.scm
./a.out

if ./a.out twice; then
    echo "double-return from callback didn't fail"
    exit 1
else
    echo "double-return from callback failed as it should."
fi

echo "======================================== runtime tests ..."
$interpret -s apply-test.scm
$compile apply-test.scm
./a.out
if ./a.out -:A10k; then
    echo "apply test with limited temp stack didn't fail"
    exit 1
else
    echo "apply test with limited temp stack failed as it should."
fi

$compile test-gc-hooks.scm
./a.out

echo "======================================== library tests ..."
$interpret -s library-tests.scm
$compile -specialize library-tests.scm
./a.out
$interpret -s records-and-setters-test.scm
$compile records-and-setters-test.scm
./a.out

echo "======================================== reader tests ..."
$interpret -s reader-tests.scm

echo "======================================== dynamic-wind tests ..."
$interpret -s dwindtst.scm >dwindtst.out
diff $DIFF_OPTS dwindtst.expected dwindtst.out
$compile dwindtst.scm
./a.out >dwindtst.out
diff $DIFF_OPTS dwindtst.expected dwindtst.out

echo "======================================== lolevel tests ..."
$interpret -s lolevel-tests.scm
$compile -specialize lolevel-tests.scm
./a.out

echo "======================================== arithmetic tests ..."
$interpret -D check -s arithmetic-test.scm

echo "======================================== pretty-printer tests ..."
$interpret -s pp-test.scm

echo "======================================== evaluation environment tests ..."
$interpret -s environment-tests.scm

echo "======================================== syntax tests ..."
$interpret -s syntax-tests.scm

echo "======================================== syntax tests (compiled) ..."
$compile syntax-tests.scm
./a.out

echo "======================================== syntax tests (v2, compiled) ..."
$compile syntax-tests-2.scm
./a.out

echo "======================================== meta-syntax tests ..."
$interpret -bnq meta-syntax-test.scm -e '(import foo)' -e "(assert (equal? '((1)) (bar 1 2)))" -e "(assert (equal? '(list 1 2 3) (listify)))" -e "(import test-import-syntax-for-syntax)" -e "(assert (equal? '(1) (test)))" -e "(import test-begin-for-syntax)" -e "(assert (equal? '(1) (test)))"
$compile_s meta-syntax-test.scm -j foo
$compile_s foo.import.scm
$interpret -bnq meta-syntax-test.scm -e '(import foo)' -e "(assert (equal? '((1)) (bar 1 2)))" -e "(assert (equal? '(list 1 2 3) (listify)))" -e "(import test-import-syntax-for-syntax)" -e "(assert (equal? '(1) (test)))" -e "(import test-begin-for-syntax)" -e "(assert (equal? '(1) (test)))"

echo "======================================== reexport tests ..."
$interpret -bnq reexport-tests.scm
$compile reexport-tests.scm
./a.out
rm -f reexport-m*.import*
$compile_s reexport-m1.scm -J
$compile_s reexport-m1.import.scm
$interpret -s reexport-m2.scm
$compile reexport-m2.scm
./a.out
$compile_s reexport-m3.scm -J
$compile_s reexport-m4.scm -J
$compile_s reexport-m5.scm -J
$compile_s reexport-m6.scm -J
$compile reexport-tests-2.scm
./a.out

echo "======================================== functor tests ..."
$interpret -bnq simple-functors-test.scm
$compile simple-functors-test.scm
./a.out
$interpret -bnq functor-tests.scm
$compile functor-tests.scm
./a.out
$compile -s square-functor.scm -J
$compile -s square-functor.import.scm
$interpret -bnq use-square-functor.scm
$compile use-square-functor.scm
./a.out
$compile -s use-square-functor.scm -J
$interpret -nqe '(require-library use-square-functor)' -e '(import sf1)' -e '(import sf2)'
rm -f sf1.import.* sf2.import.* lst.import.* mod.import.*

echo "======================================== compiler syntax tests ..."
$compile compiler-syntax-tests.scm
./a.out

echo "======================================== import tests ..."
$interpret -bnq import-tests.scm

echo "======================================== import library tests ..."
rm -f ../foo.import.* foo.import.*
$compile import-library-test1.scm -emit-import-library foo
$interpret -s import-library-test2.scm
$compile_s foo.import.scm -o foo.import.so
$interpret -s import-library-test2.scm
$compile import-library-test2.scm
./a.out
rm -f foo.import.*

echo "======================================== optionals test ..."
$interpret -s test-optional.scm
$compile test-optional.scm
./a.out

echo "======================================== syntax tests (matchable) ..."
$interpret matchable.scm -s match-test.scm

echo "======================================== syntax tests (loopy-loop) ..."
$interpret -s loopy-test.scm

echo "======================================== r4rstest ..."
echo "(expect mult-float-print-test to fail)"
$interpret -R data-structures -e '(set! ##sys#procedure->string (constantly "#<procedure>"))' \
  -i -s r4rstest.scm >r4rstest.log

diff $DIFF_OPTS r4rstest.out r4rstest.log

echo "======================================== syntax tests (r5rs_pitfalls) ..."
echo "(expect two failures)"
$interpret -i -s r5rs_pitfalls.scm

echo "======================================== r7rs tests ..."
$interpret -i -s r7rs-tests.scm

echo "======================================== module tests ..."
$interpret -include-path ${TEST_DIR}/.. -s module-tests.scm
$interpret -include-path ${TEST_DIR}/.. -s module-tests-2.scm

echo "======================================== module tests (command line options) ..."
module="test-$(date +%s)"
$compile test.scm -A -w -j "$module" -module "$module"
$interpret -e "(import-syntax $module)"
rm -f "$module.import.scm"

echo "======================================== module tests (compiled) ..."
$compile module-tests-compiled.scm
./a.out

echo "======================================== module tests (chained) ..."
rm -f m*.import.* test-chained-modules.so
$interpret -bnq test-chained-modules.scm
$compile_s test-chained-modules.scm -j m3
$compile_s m3.import.scm
$interpret -bn test-chained-modules.so
$interpret -bn test-chained-modules.so -e '(import m3) (s3)'

echo "======================================== module tests (ec) ..."
rm -f ec.so ec.import.*
$interpret -bqn ec.scm ec-tests.scm
$compile_s ec.scm -emit-import-library ec -o ec.so
$compile_s ec.import.scm -o ec.import.so 
$interpret -bnq ec.so ec-tests.scm
# $compile ec-tests.scm
# ./a.out        # takes ages to compile

echo "======================================== port tests ..."
$interpret -s port-tests.scm

echo "======================================== fixnum tests ..."
$compile fixnum-tests.scm
./a.out

echo "======================================== string->number tests ..."
$interpret -s numbers-string-conversion-tests.scm
$compile -specialize numbers-string-conversion-tests.scm
./a.out

echo "======================================== basic numeric ops tests ..."
$interpret -s numbers-test.scm
$compile -specialize numbers-test.scm
./a.out

echo "======================================== Alex Shinn's numeric ops tests ..."
$interpret -s numbers-test-ashinn.scm
$compile -specialize numbers-test-ashinn.scm
./a.out

echo "======================================== Gauche's numeric ops tests ..."
$interpret -s numbers-test-gauche.scm
$compile -specialize numbers-test-gauche.scm
./a.out

echo "======================================== srfi-4 tests ..."
$interpret -s srfi-4-tests.scm

echo "======================================== condition tests ..."
$interpret -s condition-tests.scm

echo "======================================== data-structures tests ..."
$interpret -s data-structures-tests.scm

echo "======================================== path tests ..."
$interpret -bnq path-tests.scm

echo "======================================== srfi-45 tests ..."
$interpret -s srfi-45-tests.scm

echo "======================================== posix tests ..."
$compile posix-tests.scm
./a.out

echo "======================================== find-files tests ..."
$interpret -bnq test-find-files.scm

echo "======================================== record-renaming tests ..."
$interpret -bnq record-rename-test.scm

echo "======================================== regular expression tests ..."
$interpret -bnq test-irregex.scm
$interpret -bnq test-glob.scm

echo "======================================== compiler/nursery stress test ..."
for s in 100000 120000 200000 250000 300000 350000 400000 450000 500000; do
    echo "  $s"
    ../chicken -ignore-repository ../port.scm -:s$s -output-file tmp.c -include-path ${TEST_DIR}/..
done

echo "======================================== heap literal stress test ..."
$compile heap-literal-stress-test.scm
for s in 100000 120000 200000 250000 300000 350000 400000 450000 500000; do
  echo "  $s"
  ./a.out -:d -:g -:hi$s
done

echo "======================================== symbol-GC tests ..."
$compile symbolgc-tests.scm
./a.out

echo "======================================== finalizer tests ..."
$interpret -s test-finalizers.scm
$compile test-finalizers.scm
./a.out
$compile finalizer-error-test.scm
echo "expect an error message here:"
./a.out -:hg101
$compile test-finalizers-2.scm
./a.out

echo "======================================== locative stress test ..."
$compile locative-stress-test.scm
./a.out

echo "======================================== syntax-rules stress test ..."
$time $interpret -bnq syntax-rule-stress-test.scm

echo "======================================== include test ..."
mkdir -p a/b
echo > a/b/ok.scm
echo '(include "a/b/ok.scm")' > a/b/include.scm
$compile -analyze-only a/b/include.scm
echo '(include "b/ok.scm")' > a/b/include.scm
$compile -analyze-only a/b/include.scm -include-path a
echo '(include-relative "ok.scm")' > a/b/include.scm
$compile -analyze-only a/b/include.scm
echo '(include-relative "b/ok.scm")' > a/include.scm
$compile -analyze-only a/include.scm
echo '(include-relative "b/ok.scm")' > a/b/include.scm
$compile -analyze-only a/b/include.scm -include-path a
rm -r a

echo "======================================== executable tests ..."
$compile executable-tests.scm
./a.out "$TEST_DIR/a.out"

echo "======================================== user pass tests ..."
$compile -extend user-pass-tests.scm null.scm

echo "======================================== embedding (1) ..."
$compile embedded1.c
./a.out

echo "======================================== embedding (2) ..."
$compile -e embedded2.scm
./a.out

echo "======================================== embedding (3) ..."
$compile -e embedded3.c embedded4.scm
./a.out

echo "======================================== linking tests ..."
$compile2 -unit reverser reverser/tags/1.0/reverser.scm -J -c -o reverser.o
$compile2 -link reverser linking-tests.scm
./linking-tests
$compile2 -link reverser linking-tests.scm -static
./linking-tests
cp reverser.o reverser.import.scm "$CHICKEN_INSTALL_REPOSITORY"
$compile2 -link reverser linking-tests.scm
./linking-tests
$compile2 -link reverser linking-tests.scm -static
./linking-tests

echo "======================================== private repository test ..."
mkdir -p tmp
$compile private-repository-test.scm -private-repository -o tmp/xxx
tmp/xxx ${TEST_DIR}/tmp
# This MUST be `pwd`: ${PWD} is not portable, and ${TEST_DIR} breaks mingw-msys
PATH=`pwd`/tmp:$PATH xxx ${TEST_DIR}/tmp
# this may crash, if the PATH contains a non-matching libchicken.dll on Windows:
#PATH=$PATH:${TEST_DIR}/tmp xxx ${TEST_DIR}/tmp
rm -fr reverser/*.import.* reverser/*.so

echo "======================================== done."
