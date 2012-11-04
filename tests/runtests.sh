#!/bin/sh
# runtests.sh - run CHICKEN testsuite
#
# - Note: this needs a proper shell, so it will not work with plain mingw
#   (just the compiler and the Windows shell, without MSYS)


set -e
TEST_DIR=`pwd`
OS_NAME=`uname -s`
export DYLD_LIBRARY_PATH=${TEST_DIR}/..
export LD_LIBRARY_PATH=${TEST_DIR}/..
export LIBRARY_PATH=${TEST_DIR}/..:${LIBRARY_PATH}

rm -fr test-repository
mkdir -p test-repository

# copy files into test-repository (by hand to avoid calling `chicken-install'):

for x in setup-api.so setup-api.import.so setup-download.so \
      setup-download.import.so chicken.import.so lolevel.import.so \
      srfi-1.import.so srfi-4.import.so data-structures.import.so \
      ports.import.so files.import.so posix.import.so \
      srfi-13.import.so srfi-69.import.so extras.import.so \
      irregex.import.so srfi-14.import.so tcp.import.so \
      foreign.import.so srfi-18.import.so \
      utils.import.so csi.import.so irregex.import.so types.db; do
  cp ../$x test-repository
done

CHICKEN_REPOSITORY=${TEST_DIR}/test-repository
CHICKEN=../chicken
CHICKEN_INSTALL=${TEST_DIR}/../chicken-install
CHICKEN_UNINSTALL=${TEST_DIR}/../chicken-uninstall
ASMFLAGS=
FAST_OPTIONS="-O5 -d0 -b -disable-interrupts"

TYPESDB=../types.db
cp $TYPESDB test-repository/types.db

if test -n "$MSYSTEM"; then
    CHICKEN="..\\chicken.exe"
    ASMFLAGS=-Wa,-w
    TIME=time
    # make compiled tests use proper library on Windows
    cp ../lib*chicken*.dll .
else 
    TIME=/usr/bin/time
fi


# for cygwin
if test -f ../cygchicken-0.dll; then
    cp ../cygchicken-0.dll .
    cp ../cygchicken-0.dll reverser/tags/1.0
    mv ../cygchicken-0.dll ../cygchicken-0.dll_
fi

compile="../csc -compiler $CHICKEN -v -I.. -L.. -include-path .. -o a.out"
compile2="../csc -compiler $CHICKEN -v -I.. -L.. -include-path .."
compile_s="../csc -s -compiler $CHICKEN -v -I.. -L.. -include-path .."
interpret="../csi -n -include-path .."

rm -f *.exe *.so *.o *.import.* a.out ../foo.import.*

echo "======================================== compiler tests ..."
$compile compiler-tests.scm
./a.out

echo "======================================== compiler inlining tests  ..."
$compile inlining-tests.scm -optimize-level 3
./a.out

echo "======================================== scrutiny tests ..."
$compile typematch-tests.scm -specialize -w
./a.out
$compile scrutiny-tests.scm -A -scrutinize -ignore-repository -types $TYPESDB 2>scrutiny.out -verbose

if test -n "$MSYSTEM"; then
    dos2unix scrutiny.out
fi

# this is sensitive to gensym-names, so make it optional
if test \! -f scrutiny.expected; then
    cp scrutiny.out scrutiny.expected
fi

diff -bu scrutiny.expected scrutiny.out

$compile scrutiny-tests-2.scm -A -scrutinize -analyze-only -ignore-repository -types $TYPESDB 2>scrutiny-2.out -verbose

if test -n "$MSYSTEM"; then
    dos2unix scrutiny-2.out
fi

# this is sensitive to gensym-names, so make it optional
if test \! -f scrutiny-2.expected; then
    cp scrutiny-2.out scrutiny-2.expected
fi

diff -bu scrutiny-2.expected scrutiny-2.out

$compile scrutiny-tests-3.scm -specialize -block -ignore-repository -types $TYPESDB
./a.out

$compile scrutiny-tests-strict.scm -strict-types -specialize -ignore-repository -types $TYPESDB
./a.out

echo "======================================== specialization tests ..."
rm -f foo.types foo.import.*
$compile specialization-test-1.scm -emit-type-file foo.types -specialize \
  -debug ox -emit-import-library foo
./a.out
$compile specialization-test-2.scm -types foo.types -specialize -debug ox
./a.out
rm -f foo.types foo.import.*

echo "======================================== specialization benchmark ..."
$compile fft.scm -O2 -local -d0 -disable-interrupts -b -o fft1
$compile fft.scm -O2 -local -specialize -debug x -d0 -disable-interrupts -b -o fft2 -specialize
echo "normal:"
time ./fft1 1000 7
echo "specialized:"
time ./fft2 1000 7

echo "======================================== callback tests ..."
$compile callback-tests.scm
./a.out

if ./a.out twice; then
    echo "double-return from callback didn't fail"
    exit 1
else
    echo "double-return from callback failed as it should."
fi

echo "======================================== runtime tests ..."
$interpret -s apply-test.scm
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
diff -bu dwindtst.expected dwindtst.out
$compile dwindtst.scm
./a.out >dwindtst.out
diff -bu dwindtst.expected dwindtst.out
echo "*** Skipping \"feeley-dynwind\" for now ***"
# $interpret -s feeley-dynwind.scm

echo "======================================== lolevel tests ..."
$interpret -s lolevel-tests.scm
$compile lolevel-tests.scm
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
$interpret -bnq meta-syntax-test.scm -e '(import foo)' -e "(assert (equal? '((1)) (bar 1 2)))" -e "(assert (equal? '(list 1 2 3) (listify)))"
$compile_s meta-syntax-test.scm -j foo
$compile_s foo.import.scm
$interpret -bnq -e '(require-library meta-syntax-test)' -e '(import foo)' -e "(assert (equal? '((1)) (bar 1 2)))" -e "(assert (equal? '(list 1 2 3) (listify)))"

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
$interpret -e '(set! ##sys#procedure->string (constantly "#<procedure>"))' \
  -i -s r4rstest.scm >r4rstest.log

if test -n "$MSYSTEM"; then
    # the windows runtime library prints flonums differently
    tail r4rstest.log
else
    diff -bu r4rstest.out r4rstest.log
fi

echo "======================================== syntax tests (r5rs_pitfalls) ..."
echo "(expect two failures)"
$interpret -i -s r5rs_pitfalls.scm

echo "======================================== module tests ..."
$interpret -include-path .. -s module-tests.scm
$interpret -include-path .. -s module-tests-2.scm

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

echo "======================================== hash-table tests ..."
$interpret -s hash-table-tests.scm

echo "======================================== port tests ..."
$interpret -s port-tests.scm

echo "======================================== fixnum tests ..."
$compile fixnum-tests.scm
./a.out

echo "======================================== string->number tests ..."
$compile numbers-string-conversion-tests.scm
./a.out

echo "======================================== srfi-4 tests ..."
$interpret -s srfi-4-tests.scm

echo "======================================== srfi-13 tests ..."
$interpret -s srfi-13-tests.scm

echo "======================================== srfi-14 tests ..."
$compile srfi-14-tests.scm
./a.out

echo "======================================== condition tests ..."
$interpret -s condition-tests.scm

echo "======================================== srfi-18 tests ..."
$interpret -s simple-thread-test.scm
$interpret -s mutex-test.scm

echo "======================================== data-structures tests ..."
$interpret -s data-structures-tests.scm

echo "======================================== path tests ..."
$interpret -bnq path-tests.scm

echo "======================================== posix tests ..."
$compile posix-tests.scm
./a.out
rm -fr tmpdir
mkdir tmpdir
touch tmpdir/.dotfile

if test -z "$MSYSTEM"; then
    ln -s /usr tmpdir/symlink
fi

$interpret -R posix -e '(delete-directory "tmpdir" #t)'

echo "======================================== signal tests ..."
$compile signal-tests.scm
./a.out

echo "======================================== lolevel tests ..."
$interpret -s lolevel-tests.scm
$compile lolevel-tests.scm
./a.out

echo "======================================== regular expression tests ..."
$interpret -bnq test-irregex.scm
$interpret -bnq test-glob.scm

echo "======================================== compiler/nursery stress test ..."
for s in 100000 120000 200000 250000 300000 350000 400000 450000 500000; do
    echo "  $s"
    ../chicken ../utils.scm -:s$s -output-file tmp.c -include-path .. 
done

echo "======================================== symbol-GC tests ..."
$compile symbolgc-tests.scm
./a.out -:w

echo "======================================== finalizer tests ..."
$interpret -s test-finalizers.scm
$compile finalizer-error-test.scm
echo "expect an error message here:"
./a.out -:hg101
$compile test-finalizers-2.scm
./a.out

echo "======================================== locative stress test ..."
$compile locative-stress-test.scm
./a.out

echo "======================================== syntax-rules stress test ..."
time $interpret -bnq syntax-rule-stress-test.scm

echo "======================================== embedding (1) ..."
$compile embedded1.c
./a.out

echo "======================================== embedding (2) ..."
$compile -e embedded2.scm
./a.out

echo "======================================== embedding (3) ..."
$compile -e embedded3.c embedded4.scm
./a.out

echo "======================================== private repository test ..."
mkdir -p tmp
$compile private-repository-test.scm -private-repository -o tmp/xxx
tmp/xxx $PWD/tmp
PATH=$PWD/tmp:$PATH xxx $PWD/tmp
# this may crash, if the PATH contains a non-matching libchicken.dll on Windows:
#PATH=$PATH:$PWD/tmp xxx $PWD/tmp
rm -fr rev-app rev-app-2 reverser/*.import.* reverser/*.so

echo "======================================== reinstall tests"
CHICKEN_REPOSITORY=$CHICKEN_REPOSITORY $CHICKEN_UNINSTALL -force reverser
CHICKEN_REPOSITORY=$CHICKEN_REPOSITORY $CHICKEN_INSTALL -t local -l $TEST_DIR reverser:1.0 \
 -csi ${TEST_DIR}/../csi
CHICKEN_REPOSITORY=$CHICKEN_REPOSITORY $interpret -bnq rev-app.scm 1.0
CHICKEN_REPOSITORY=$CHICKEN_REPOSITORY $CHICKEN_INSTALL -t local -l $TEST_DIR -reinstall -force \
 -csi ${TEST_DIR}/../csi
CHICKEN_REPOSITORY=$CHICKEN_REPOSITORY $interpret -bnq rev-app.scm 1.0

echo "======================================== deployment tests"
mkdir rev-app
CHICKEN_REPOSITORY=$CHICKEN_REPOSITORY $CHICKEN_INSTALL -t local -l $TEST_DIR reverser
CHICKEN_REPOSITORY=$CHICKEN_REPOSITORY $compile2 -deploy rev-app.scm
CHICKEN_REPOSITORY=$CHICKEN_REPOSITORY $CHICKEN_INSTALL -deploy -prefix rev-app -t local -l $TEST_DIR reverser
unset LD_LIBRARY_PATH DYLD_LIBRARY_PATH CHICKEN_REPOSITORY
rev-app/rev-app 1.1
mv rev-app rev-app-2
rev-app-2/rev-app 1.1

echo "======================================== done."
