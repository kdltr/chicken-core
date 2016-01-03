@echo off
@rem runtests.bat - run CHICKEN testsuite on cmd.exe with mingw

setlocal enableextensions

set TEST_DIR=%cd%
set OS_NAME=WindowsNT

set CHICKEN=..\chicken
set CHICKEN_PROFILE=..\chicken-profile
set ASMFLAGS=-Wa,-w
set FAST_OPTIONS=-O5 -d0 -b -disable-interrupts
set PATH=%cd%\..;%PATH%

set TYPESDB=..\types.db
rem Increase this when tests start failing on "inexplicable" diffs
set FCBUFSIZE=500

set compile=..\csc -types %TYPESDB% -ignore-repository -compiler %CHICKEN% -v -I%TEST_DIR%/.. -L%TEST_DIR%/.. -include-path %TEST_DIR%/.. -o a.out
set compile2=..\csc -compiler %CHICKEN% -v -I%TEST_DIR%/.. -L%TEST_DIR%/.. -include-path %TEST_DIR%/..
set compile_s=..\csc -s -types %TYPESDB% -ignore-repository -compiler %CHICKEN% -v -I%TEST_DIR%/.. -L%TEST_DIR%/.. -include-path %TEST_DIR%/..
set interpret=..\csi -n -include-path %TEST_DIR%/..

del /f /q *.exe *.so *.o *.import.* ..\foo.import.*

echo ======================================== version tests ...
%interpret% -s version-tests.scm
if errorlevel 1 exit /b 1

echo ======================================== compiler tests ...
%compile% compiler-tests.scm
if errorlevel 1 exit /b 1
a.out
if errorlevel 1 exit /b 1

echo ======================================== compiler inlining tests  ...
%compile% inlining-tests.scm -optimize-level 3
if errorlevel 1 exit /b 1
a.out
if errorlevel 1 exit /b 1

echo ======================================== optimizer tests  ...
%compile% clustering-tests.scm -clustering
if errorlevel 1 exit /b 1
a.out
if errorlevel 1 exit /b 1

echo ======================================== profiler tests ...
%compile% null.scm -profile -profile-name TEST.profile
a.out
%CHICKEN_PROFILE% TEST.profile
if errorlevel 1 exit /b 1

echo ======================================== scrutiny tests ...
%compile% typematch-tests.scm -specialize -w
if errorlevel 1 exit /b 1
a.out
if errorlevel 1 exit /b 1
%compile% scrutiny-tests.scm -A -verbose 2>scrutiny.out
if errorlevel 1 exit /b 1

rem this is sensitive to gensym-names, so make it optional
if not exist scrutiny.expected copy /Y scrutiny.out scrutiny.expected

fc /lb%FCBUFSIZE% /w scrutiny.expected scrutiny.out
if errorlevel 1 exit /b 1

%compile% scrutiny-tests-2.scm -A -verbose 2>scrutiny-2.out
if errorlevel 1 exit /b 1

if not exist scrutiny-2.expected copy /Y scrutiny-2.out scrutiny-2.expected
fc /lb%FCBUFSIZE% /w scrutiny-2.expected scrutiny-2.out
if errorlevel 1 exit /b 1

%compile% scrutiny-tests-3.scm -specialize -block
if errorlevel 1 exit /b 1
a.out
if errorlevel 1 exit /b 1

%compile% scrutiny-tests-strict.scm -strict-types -specialize
if errorlevel 1 exit /b 1
a.out
if errorlevel 1 exit /b 1

echo ======================================== specialization tests ...
del /f /q foo.types foo.import.*
%compile% specialization-test-1.scm -emit-type-file foo.types -specialize -debug ox -emit-import-library foo
if errorlevel 1 exit /b 1
a.out
if errorlevel 1 exit /b 1
%compile% specialization-test-2.scm -types foo.types -types specialization-test-2.types -specialize -debug ox
if errorlevel 1 exit /b 1
a.out
if errorlevel 1 exit /b 1
del /f /q foo.types foo.import.*

echo ======================================== specialization benchmark ...
%compile% fft.scm -O2 -local -d0 -disable-interrupts -b -o fft1
if errorlevel 1 exit /b 1
%compile% fft.scm -O2 -local -specialize -debug x -d0 -disable-interrupts -b -o fft2 -specialize
if errorlevel 1 exit /b 1
echo normal:
fft1 1000 7
if errorlevel 1 exit /b 1
echo specialized:
fft2 1000 7
if errorlevel 1 exit /b 1

echo ======================================== callback tests ...
%compile% callback-tests.scm
if errorlevel 1 exit /b 1
a.out
if errorlevel 1 exit /b 1
a.out twice

if errorlevel 1 (
  echo double-return from callback failed as it should.
) else (
  echo double-return from callback didn't fail
  exit /b 1
)

echo ======================================== runtime tests ...
%interpret% -s apply-test.scm
if errorlevel 1 exit /b 1
%compile% apply-test.scm
if errorlevel 1 exit /b 1
a.out
if errorlevel 1 exit /b 1
%compile% test-gc-hooks.scm
if errorlevel 1 exit /b 1
a.out
if errorlevel 1 exit /b 1

echo ======================================== library tests ...
%interpret% -s library-tests.scm
if errorlevel 1 exit /b 1
%compile% -specialize library-tests.scm
if errorlevel 1 exit /b 1
a.out
if errorlevel 1 exit /b 1
%interpret% -s records-and-setters-test.scm
if errorlevel 1 exit /b 1
%compile% records-and-setters-test.scm
if errorlevel 1 exit /b 1
a.out
if errorlevel 1 exit /b 1

echo ======================================== reader tests ...
%interpret% -s reader-tests.scm
if errorlevel 1 exit /b 1

echo ======================================== dynamic-wind tests ...
%interpret% -s dwindtst.scm >dwindtst.out
if errorlevel 1 exit /b 1
fc /lb%FCBUFSIZE% /w dwindtst.expected dwindtst.out
if errorlevel 1 exit /b 1
%compile% dwindtst.scm
if errorlevel 1 exit /b 1
a.out >dwindtst.out
if errorlevel 1 exit /b 1
fc /lb%FCBUFSIZE% /w dwindtst.expected dwindtst.out
if errorlevel 1 exit /b 1

echo ======================================== lolevel tests ...
%interpret% -s lolevel-tests.scm
if errorlevel 1 exit /b 1
%compile% lolevel-tests.scm
if errorlevel 1 exit /b 1
a.out
if errorlevel 1 exit /b 1

echo ======================================== arithmetic tests ...
%interpret% -D check -s arithmetic-test.scm
if errorlevel 1 exit /b 1

echo ======================================== pretty-printer tests ...
%interpret% -s pp-test.scm
if errorlevel 1 exit /b 1

echo ======================================== evaluation environment tests ...
%interpret% -s environment-tests.scm
if errorlevel 1 exit /b 1

echo ======================================== syntax tests ...
%interpret% -s syntax-tests.scm
if errorlevel 1 exit /b 1

echo ======================================== syntax tests (compiled) ...
%compile% syntax-tests.scm
a.out
if errorlevel 1 exit /b 1

echo ======================================== syntax tests (v2, compiled) ...
%compile% syntax-tests-2.scm
if errorlevel 1 exit /b 1
a.out
if errorlevel 1 exit /b 1

echo ======================================== meta-syntax tests ...
%interpret% -bnq meta-syntax-test.scm -e "(import foo)" -e "(assert (equal? '((1)) (bar 1 2)))" -e "(assert (equal? '(list 1 2 3) (listify)))"
if errorlevel 1 exit /b 1
%compile_s% meta-syntax-test.scm -j foo
if errorlevel 1 exit /b 1
%compile_s% foo.import.scm
if errorlevel 1 exit /b 1
%interpret% -bnq -e "(require-library meta-syntax-test)" -e "(import foo)" -e "(assert (equal? '((1)) (bar 1 2)))" -e "(assert (equal? '(list 1 2 3) (listify)))"
if errorlevel 1 exit /b 1

echo ======================================== reexport tests ...
%interpret% -bnq reexport-tests.scm
if errorlevel 1 exit /b 1
%compile% reexport-tests.scm
if errorlevel 1 exit /b 1
a.out
if errorlevel 1 exit /b 1
del /f /q reexport-m*.import*
%compile_s% reexport-m1.scm -J
if errorlevel 1 exit /b 1
%compile_s% reexport-m1.import.scm
if errorlevel 1 exit /b 1
%interpret% -s reexport-m2.scm
if errorlevel 1 exit /b 1
%compile% reexport-m2.scm
if errorlevel 1 exit /b 1
a.out
if errorlevel 1 exit /b 1
%compile_s% reexport-m3.scm -J
if errorlevel 1 exit /b 1
%compile_s% reexport-m4.scm -J
if errorlevel 1 exit /b 1
%compile_s% reexport-m5.scm -J
if errorlevel 1 exit /b 1
%compile_s% reexport-m6.scm -J
if errorlevel 1 exit /b 1
%compile% reexport-tests-2.scm
if errorlevel 1 exit /b 1
a.out
if errorlevel 1 exit /b 1


echo ======================================== functor tests ...
%interpret% -bnq simple-functors-test.scm
if errorlevel 1 exit /b 1
%compile% simple-functors-test.scm
if errorlevel 1 exit /b 1
a.out
if errorlevel 1 exit /b 1
%interpret% -bnq functor-tests.scm
if errorlevel 1 exit /b 1
%compile% functor-tests.scm
if errorlevel 1 exit /b 1
a.out
if errorlevel 1 exit /b 1
%compile% -s square-functor.scm -J
if errorlevel 1 exit /b 1
%compile% -s square-functor.import.scm
if errorlevel 1 exit /b 1
%interpret% -bnq use-square-functor.scm
if errorlevel 1 exit /b 1
%compile% use-square-functor.scm
if errorlevel 1 exit /b 1
a.out
if errorlevel 1 exit /b 1
%compile% -s use-square-functor.scm -J
if errorlevel 1 exit /b 1
%interpret% -nqe "(require-library use-square-functor)" -e "(import sf1)" -e "(import sf2)"
if errorlevel 1 exit /b 1
del /f /q sf1.import.* sf2.import.* lst.import.* mod.import.*

echo ======================================== compiler syntax tests ...
%compile% compiler-syntax-tests.scm
if errorlevel 1 exit /b 1
a.out
if errorlevel 1 exit /b 1

echo ======================================== import tests ...
%interpret% -bnq import-tests.scm
if errorlevel 1 exit /b 1

echo ======================================== import library tests ...
del /f /q ..\foo.import.* foo.import.*
%compile% import-library-test1.scm -emit-import-library foo
if errorlevel 1 exit /b 1
%interpret% -s import-library-test2.scm
if errorlevel 1 exit /b 1
%compile_s% foo.import.scm -o foo.import.so
if errorlevel 1 exit /b 1
%interpret% -s import-library-test2.scm
if errorlevel 1 exit /b 1
%compile% import-library-test2.scm
if errorlevel 1 exit /b 1
a.out
if errorlevel 1 exit /b 1
del /f /q foo.import.*

echo ======================================== optionals test ...
%interpret% -s test-optional.scm
if errorlevel 1 exit /b 1
%compile% test-optional.scm
if errorlevel 1 exit /b 1
a.out
if errorlevel 1 exit /b 1

echo ======================================== syntax tests (matchable) ...
%interpret% matchable.scm -s match-test.scm
if errorlevel 1 exit /b 1

echo ======================================== syntax tests (loopy-loop) ...
%interpret% -s loopy-test.scm
if errorlevel 1 exit /b 1

echo ======================================== r4rstest ...
echo (expect mult-float-print-test to fail)
%interpret% -R data-structures -e "(set! ##sys#procedure->string (constantly \"#<procedure>\"))" -i -s r4rstest.scm >r4rstest.log
if errorlevel 1 exit /b 1

type r4rstest.log

echo ======================================== syntax tests (r5rs_pitfalls) ...
echo (expect two failures)
%interpret% -i -s r5rs_pitfalls.scm
if errorlevel 1 exit /b 1

echo "======================================== r7rs tests ..."
echo (expect two failures)
%interpret% -i -s r7rs-tests.scm
if errorlevel 1 exit /b 1

echo ======================================== module tests ...
%interpret% -include-path %TEST_DIR%/.. -s module-tests.scm
if errorlevel 1 exit /b 1
%interpret% -include-path %TEST_DIR%/.. -s module-tests-2.scm
if errorlevel 1 exit /b 1

echo ======================================== module tests (command line options) ...
set module="test"
%compile% test.scm -w -A -j %module% -module %module%
if errorlevel 1 exit /b 1
%interpret% -e "(import-syntax %module%)"
if errorlevel 1 exit /b 1
del /f /q %module%.import.scm

echo ======================================== module tests (compiled) ...
%compile% module-tests-compiled.scm
if errorlevel 1 exit /b 1
a.out
if errorlevel 1 exit /b 1

echo ======================================== module tests (chained) ...
del /f /q m*.import.* test-chained-modules.so
%interpret% -bnq test-chained-modules.scm
if errorlevel 1 exit /b 1
%compile_s% test-chained-modules.scm -j m3
if errorlevel 1 exit /b 1
%compile_s% m3.import.scm
if errorlevel 1 exit /b 1
%interpret% -bn test-chained-modules.so
if errorlevel 1 exit /b 1
%interpret% -bn test-chained-modules.so -e "(import m3) (s3)"
if errorlevel 1 exit /b 1

echo ======================================== module tests (ec) ...
del /f /q ec.so ec.import.*
%interpret% -bqn ec.scm ec-tests.scm
if errorlevel 1 exit /b 1
%compile_s% ec.scm -emit-import-library ec -o ec.so
if errorlevel 1 exit /b 1
%compile_s% ec.import.scm -o ec.import.so 
if errorlevel 1 exit /b 1
%interpret% -bnq ec.so ec-tests.scm
rem %compile% ec-tests.scm
rem a.out        # takes ages to compile

echo ======================================== port tests ...
%interpret% -s port-tests.scm
if errorlevel 1 exit /b 1

echo ======================================== fixnum tests ...
%compile% fixnum-tests.scm
if errorlevel 1 exit /b 1
a.out
if errorlevel 1 exit /b 1

echo ======================================== string->number tests ...
%interpret% -s numbers-string-conversion-tests.scm
if errorlevel 1 exit /b 1
%compile% -specialize numbers-string-conversion-tests.scm
if errorlevel 1 exit /b 1
a.out
if errorlevel 1 exit /b 1

echo ======================================== basic numeric ops tests ...
%interpret% -s numbers-test.scm
if errorlevel 1 exit /b 1
%compile% -specialize numbers-test.scm
if errorlevel 1 exit /b 1
a.out
if errorlevel 1 exit /b 1

echo ======================================== Alex Shinns numeric ops tests ...
%interpret% -s numbers-test-ashinn.scm
if errorlevel 1 exit /b 1
%compile% -specialize numbers-test-ashinn.scm
if errorlevel 1 exit /b 1
a.out
if errorlevel 1 exit /b 1

echo ======================================== Gauche numeric ops tests ...
%interpret% -s numbers-test-gauche.scm
if errorlevel 1 exit /b 1
%compile% -specialize numbers-test-gauche.scm
if errorlevel 1 exit /b 1
a.out
if errorlevel 1 exit /b 1

echo ======================================== srfi-4 tests ...
%interpret% -s srfi-4-tests.scm
if errorlevel 1 exit /b 1

echo ======================================== condition tests ...
%interpret% -s condition-tests.scm
if errorlevel 1 exit /b 1

echo ======================================== data-structures tests ...
%interpret% -s data-structures-tests.scm
if errorlevel 1 exit /b 1

echo ======================================== path tests ...
%interpret% -bnq path-tests.scm
if errorlevel 1 exit /b 1

echo ======================================== srfi-45 tests ...
%interpret% -s srfi-45-tests.scm
if errorlevel 1 exit /b 1

echo ======================================== posix tests ...
%compile% posix-tests.scm
if errorlevel 1 exit /b 1
a.out
if errorlevel 1 exit /b 1
del /f /q /s tmpdir
mkdir tmpdir
echo 0 >tmpdir\.dotfile
%interpret% -R posix -e "(delete-directory \"tmpdir\" #t)"
if errorlevel 1 exit /b 1

echo ======================================== find-files tests ...
%interpret% -bnq test-find-files.scm
if errorlevel 1 exit /b 1

echo ======================================== regular expression tests ...
%interpret% -bnq test-irregex.scm
if errorlevel 1 exit /b 1
%interpret% -bnq test-glob.scm
if errorlevel 1 exit /b 1

echo ======================================== compiler/nursery stress test ...
for %%s in (100000 120000 200000 250000 300000 350000 400000 450000 500000) do (
  echo %%s
  ..\chicken -ignore-repository ..\utils.scm -:s%%s -output-file tmp.c -include-path %TEST_DIR%/.. 
  if errorlevel 1 exit /b 1
)

echo ======================================== heap literal stress test ...
%compile% heap-literal-stress-test.scm
if errorlevel 1 exit /b 1

for %%s in (100000 120000 200000 250000 300000 350000 400000 450000 500000) do (
  echo %%s
  a.out -:hi%%s
  if errorlevel 1 exit /b 1
)


echo ======================================== symbol-GC tests ...
%compile% symbolgc-tests.scm
if errorlevel 1 exit /b 1
a.out -:w
rem Currently disabled, because this may leave 1 symbol unreclaimed.
rem if errorlevel 1 exit /b 1

echo ======================================== finalizer tests ...
%interpret% -s test-finalizers.scm
if errorlevel 1 exit /b 1
%compile% test-finalizers.scm
if errorlevel 1 exit /b 1
a.out
if errorlevel 1 exit /b 1

echo ======================================== finalizer tests (2) ...
%compile% finalizer-error-test.scm
if errorlevel 1 exit /b 1
a.out -:hg101
if errorlevel 1 exit /b 1
%compile% test-finalizers-2.scm
if errorlevel 1 exit /b 1
a.out
if errorlevel 1 exit /b 1

echo ======================================== locative stress test ...
%compile% locative-stress-test.scm
if errorlevel 1 exit /b 1
a.out
if errorlevel 1 exit /b 1

echo ======================================== syntax-rules stress test ...
%interpret% -bnq syntax-rule-stress-test.scm

echo "======================================== executable tests ..."
%compile% executable-tests.scm
if errorlevel 1 exit /b 1
a.out %TEST_DIR%\a.out
if errorlevel 1 exit /b 1

echo ======================================== embedding (1) ...
%compile% embedded1.c
if errorlevel 1 exit /b 1
a.out
if errorlevel 1 exit /b 1

echo ======================================== embedding (2) ...
%compile% -e embedded2.scm
if errorlevel 1 exit /b 1
a.out
if errorlevel 1 exit /b 1

echo ======================================== embedding (3) ...
%compile% -e embedded3.c embedded4.scm
if errorlevel 1 exit /b 1
a.out
if errorlevel 1 exit /b 1

echo ======================================== private repository test ...
del /f /s /q tmp
mkdir tmp
%compile% private-repository-test.scm -private-repository -o tmp\xxx
if errorlevel 1 exit /b 1
tmp\xxx %CD%\tmp
set PATH=%CD%\tmp;%PATH% xxx %CD%\tmp
rem this may crash, if the PATH contains a non-matching libchicken.dll on Windows:
set PATH=%PATH%;%CD%\tmp xxx %CD%\tmp
del /f /q /s rev-app rev-app-2 reverser\*.import.* reverser\*.so

rem echo ======================================== reinstall tests
rem currently disabled for windows

rem echo ======================================== deployment tests
rem currently disabled for windows

echo ======================================== done.
