# rules.make - basic build rules -*- Makefile -*-
#
# Copyright (c) 2008-2013, The Chicken Team
# Copyright (c) 2000-2007, Felix L. Winkelmann
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following
# conditions are met:
#
#   Redistributions of source code must retain the above copyright notice, this list of conditions and the following
#     disclaimer. 
#   Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following
#     disclaimer in the documentation and/or other materials provided with the distribution. 
#   Neither the name of the author nor the names of its contributors may be used to endorse or promote
#     products derived from this software without specific prior written permission. 
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS
# OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
# AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR
# CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
# CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROd	CUREMENT OF SUBSTITUTE GOODS OR
# SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
# THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
# OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.

VPATH=$(SRCDIR)

# Clear Make's default rules for C programs
.SUFFIXES:
%.o : %.c
%: %.o

# object files

SETUP_API_OBJECTS_1 = setup-api setup-download

LIBCHICKEN_SCHEME_OBJECTS_1 = \
       library eval data-structures ports files extras lolevel utils tcp srfi-1 srfi-4 srfi-13 \
       srfi-14 srfi-18 srfi-69 $(POSIXFILE) irregex scheduler \
       profiler stub expand modules chicken-syntax chicken-ffi-syntax build-version
LIBCHICKEN_OBJECTS_1 = $(LIBCHICKEN_SCHEME_OBJECTS_1) runtime
LIBCHICKEN_SHARED_OBJECTS = $(LIBCHICKEN_OBJECTS_1:=$(O))
LIBCHICKEN_STATIC_OBJECTS = $(LIBCHICKEN_OBJECTS_1:=-static$(O))

COMPILER_OBJECTS_1 = \
	chicken batch-driver compiler optimizer compiler-syntax scrutinizer support \
	c-platform c-backend
COMPILER_OBJECTS        = $(COMPILER_OBJECTS_1:=$(O))
COMPILER_STATIC_OBJECTS = $(COMPILER_OBJECTS_1:=-static$(O))

# "Utility programs" is arbitrary. It includes anything but the "chicken" binary.
# We can't use the INSTALLED_PROGRAMS below because of the possible $(EXE)
# suffix and other possible mangling requested by the user. (is this supported?)
UTILITY_PROGRAM_OBJECTS_1 = \
	csc csi chicken-install chicken-uninstall chicken-status chicken-profile

ALWAYS_STATIC_UTILITY_PROGRAM_OBJECTS_1 = \
	chicken-bug

## TODO: Shouldn't these manpages match their program names (ie CSI_PROGRAM etc)?
MANPAGES = \
	chicken.1 csc.1 csi.1 chicken-install.1 chicken-uninstall.1 \
	chicken-status.1 chicken-profile.1 chicken-bug.1

# Not all programs built are installed(?) This is the master list that takes
# care of which programs should actually be installed/uninstalled
INSTALLED_PROGRAMS = \
	$(CHICKEN_PROGRAM) $(CSI_PROGRAM) $(CHICKEN_PROFILE_PROGRAM) \
	$(CSC_PROGRAM) $(CHICKEN_BUG_PROGRAM)

ifndef STATICBUILD
INSTALLED_PROGRAMS += $(CHICKEN_STATUS_PROGRAM) \
	$(CHICKEN_INSTALL_PROGRAM) $(CHICKEN_UNINSTALL_PROGRAM)
endif

# These generated files make up a bootstrapped distribution build.
# They are not cleaned by the 'clean' target, but only by 'spotless'.
DISTFILES = $(filter-out runtime.c,$(LIBCHICKEN_OBJECTS_1:=.c)) \
	$(UTILITY_PROGRAM_OBJECTS_1:=.c) \
	$(ALWAYS_STATIC_UTILITY_PROGRAM_OBJECTS_1:=.c) \
	$(COMPILER_OBJECTS_1:=.c) \
	$(SETUP_API_OBJECTS_1:=.c) \
	$(SETUP_API_OBJECTS_1:=.import.scm) $(SETUP_API_OBJECTS_1:=.import.c) \
	$(IMPORT_LIBRARIES:=.import.c) \
	posixunix.c posixwin.c
# Remove the duplicate $(POSIXFILE) entry:
DISTFILES := $(sort $(DISTFILES))

# library objects

## Any variable that starts with "declare-" is a meta-rule. When $(call)ed
## it produces output that represents an instantiated rule and recipe.
## This output then needs to be $(eval)ed in order to be added to the
## ruleset evaluated by Make.  This allows us to automatically generate
## similar rules for long lists of targets.

## Note: in some of the rules that follow it is important to add
## $(INCLUDES) last, because on raw mingw (using the DOS shell) the
## backslash in "-I.\" seems to be interpreted as a line-terminator.
## This may be caused by cmd.exe-stupidness or a bug in mingw32-make
## or some other obscure reason.

define declare-shared-library-object
$(1)$(O): $(1).c chicken.h $$(CHICKEN_CONFIG_H)
	$$(C_COMPILER) $$(C_COMPILER_OPTIONS) \
	  $$(C_COMPILER_COMPILE_OPTION) $$(C_COMPILER_OPTIMIZATION_OPTIONS) $$(C_COMPILER_SHARED_OPTIONS) \
	  $$(C_COMPILER_BUILD_RUNTIME_OPTIONS) $$< $$(C_COMPILER_OUTPUT) \
	  $$(INCLUDES)
endef

# The above meta-rule is reused in the setup API stuff below, so we alias it
declare-libchicken-object = $(declare-shared-library-object)

$(foreach obj, $(LIBCHICKEN_OBJECTS_1),\
          $(eval $(call declare-libchicken-object,$(obj))))

# static versions

define declare-static-library-object
$(1)-static$(O): $(1).c chicken.h $$(CHICKEN_CONFIG_H)
	$$(C_COMPILER) $$(C_COMPILER_OPTIONS) \
	  $$(C_COMPILER_COMPILE_OPTION) $$(C_COMPILER_OPTIMIZATION_OPTIONS) \
	  $$(C_COMPILER_STATIC_OPTIONS) \
	  $$(C_COMPILER_BUILD_RUNTIME_OPTIONS) $$< $$(C_COMPILER_OUTPUT) \
	  $$(INCLUDES)
endef

declare-static-libchicken-object = $(declare-static-library-object)

$(foreach obj, $(LIBCHICKEN_OBJECTS_1),\
          $(eval $(call declare-static-libchicken-object,$(obj))))

# import library objects

define declare-import-lib-object
$(1).import$(O): $(1).import.c chicken.h $$(CHICKEN_CONFIG_H)
	$$(C_COMPILER) $$(C_COMPILER_OPTIONS) $$(C_COMPILER_PTABLES_OPTIONS) \
	  -DC_SHARED $$(C_COMPILER_COMPILE_OPTION) \
	  $$(C_COMPILER_OPTIMIZATION_OPTIONS) $$(C_COMPILER_SHARED_OPTIONS) \
	  $$(C_COMPILER_BUILD_RUNTIME_OPTIONS) $$< $$(C_COMPILER_OUTPUT) \
	  $$(INCLUDES)
endef

$(foreach obj,$(IMPORT_LIBRARIES),\
          $(eval $(call declare-import-lib-object,$(obj))))

# setup extension objects

declare-setup-api-object = $(declare-shared-library-object)

$(foreach obj,$(SETUP_API_OBJECTS_1),\
          $(eval $(call declare-setup-api-object,$(obj))))

# compiler objects

define declare-compiler-object
$(1)$(O): $(1).c chicken.h $$(CHICKEN_CONFIG_H)
	$$(C_COMPILER) $$(C_COMPILER_OPTIONS) \
	  $$(C_COMPILER_COMPILE_OPTION) $$(C_COMPILER_OPTIMIZATION_OPTIONS) $$(C_COMPILER_SHARED_OPTIONS) $$< \
	  $$(C_COMPILER_OUTPUT) $$(INCLUDES)
endef

$(foreach obj, $(COMPILER_OBJECTS_1),\
          $(eval $(call declare-compiler-object,$(obj))))

# static compiler objects

define declare-static-compiler-object
$(1)-static$(O): $(1).c chicken.h $$(CHICKEN_CONFIG_H)
	$$(C_COMPILER) $$(C_COMPILER_OPTIONS) \
	  $$(C_COMPILER_STATIC_OPTIONS) \
	  $$(C_COMPILER_COMPILE_OPTION) $$(C_COMPILER_OPTIMIZATION_OPTIONS) $$< $$(C_COMPILER_OUTPUT) \
	  $$(INCLUDES)
endef

$(foreach obj, $(COMPILER_OBJECTS_1),\
          $(eval $(call declare-static-compiler-object,$(obj))))

# assembler objects

ifneq ($(HACKED_APPLY),)
$(APPLY_HACK_OBJECT): $(SRCDIR)apply-hack.$(ARCH)$(ASM)
	$(ASSEMBLER) $(ASSEMBLER_OPTIONS) $(ASSEMBLER_COMPILE_OPTION) $< $(ASSEMBLER_OUTPUT)
endif

# program objects

define declare-utility-program-object
$(1)$(O): $(1).c chicken.h $$(CHICKEN_CONFIG_H)
	$$(C_COMPILER) $$(C_COMPILER_OPTIONS) $$(C_COMPILER_SHARED_OPTIONS) \
	  $$(C_COMPILER_COMPILE_OPTION) $$(C_COMPILER_OPTIMIZATION_OPTIONS) $$< $$(C_COMPILER_OUTPUT) \
	 $$(INCLUDES) 
endef

$(foreach obj, $(UTILITY_PROGRAM_OBJECTS_1),\
          $(eval $(call declare-utility-program-object,$(obj))))


# static program objects

define declare-always-static-utility-program-object
$(1)$(O): $(1).c chicken.h $$(CHICKEN_CONFIG_H)
	$$(C_COMPILER) $$(C_COMPILER_OPTIONS) \
	  $$(C_COMPILER_STATIC_OPTIONS) \
	  $$(C_COMPILER_COMPILE_OPTION) $$(C_COMPILER_OPTIMIZATION_OPTIONS) $$< $$(C_COMPILER_OUTPUT) \
	  $$(INCLUDES)
endef

$(foreach obj, $(ALWAYS_STATIC_UTILITY_PROGRAM_OBJECTS_1),\
          $(eval $(call declare-always-static-utility-program-object,$(obj))))

# resource objects

%.rc.o: %.rc
	$(RC_COMPILER) $< $@

# libraries

.PHONY: libs

libs: $(TARGETLIBS)

lib$(PROGRAM_PREFIX)chicken$(PROGRAM_SUFFIX)$(SO): $(LIBCHICKEN_SHARED_OBJECTS) $(APPLY_HACK_OBJECT)
	$(LINKER) $(LINKER_OPTIONS) $(LINKER_LINK_SHARED_LIBRARY_OPTIONS) $(LIBCHICKEN_SO_LINKER_OPTIONS) \
	  $(LINKER_OUTPUT) $^ $(LIBCHICKEN_SO_LIBRARIES)
ifdef USES_SONAME
	ln -sf $(LIBCHICKEN_SO_FILE) $(LIBCHICKEN_SO_FILE).$(BINARYVERSION)
endif

cyg$(PROGRAM_PREFIX)chicken$(PROGRAM_SUFFIX)-0.dll: $(LIBCHICKEN_SHARED_OBJECTS) $(APPLY_HACK_OBJECT)
	$(LINKER) -shared -o $(LIBCHICKEN_SO_FILE) -Wl,--dll -Wl,--add-stdcall-alias \
	    -Wl,--enable-stdcall-fixup -Wl,--warn-unresolved-symbols \
	    -Wl,--dll-search-prefix=cyg -Wl,--allow-multiple-definition \
	    -Wl,--allow-shlib-undefined \
	    -Wl,--out-implib=libchicken.dll.a -Wl,--export-all-symbols \
	    -Wl,--enable-auto-import \
	    -Wl,--whole-archive $(LIBCHICKEN_SHARED_OBJECTS) $(APPLY_HACK_OBJECT) \
	    -Wl,--no-whole-archive $(LIBCHICKEN_SO_LIBRARIES)

lib$(PROGRAM_PREFIX)chicken$(PROGRAM_SUFFIX)$(A): $(APPLY_HACK_OBJECT) $(LIBCHICKEN_STATIC_OBJECTS)
	$(LIBRARIAN) $(LIBRARIAN_OPTIONS) $(LIBRARIAN_OUTPUT) $^

# import libraries and extensions

%.so: %.o
	$(LINKER) $(LINKER_OPTIONS) $(LINKER_LINK_SHARED_DLOADABLE_OPTIONS) $^ $(LINKER_OUTPUT_OPTION) $@ \
	  $(LINKER_LIBRARY_PREFIX)$(PROGRAM_PREFIX)chicken$(PROGRAM_SUFFIX)$(LINKER_LIBRARY_SUFFIX) \
	  $(LIBRARIES)

# executables

$(CHICKEN_SHARED_EXECUTABLE): $(COMPILER_OBJECTS) $(PRIMARY_LIBCHICKEN)
	$(LINKER) $(LINKER_OPTIONS) $(LINKER_EXECUTABLE_OPTIONS) $(COMPILER_OBJECTS) $(LINKER_OUTPUT) \
          $(LINKER_LIBRARY_PREFIX)$(PROGRAM_PREFIX)chicken$(PROGRAM_SUFFIX)$(LINKER_LIBRARY_SUFFIX) $(LINKER_LINK_SHARED_PROGRAM_OPTIONS) $(LIBRARIES)

define declare-program-from-object
$(1)-RC_FILE = $(if $(and $(RC_COMPILER),$(3)),$(2).rc$(O))

$(1): $(2)$(O) $$(PRIMARY_LIBCHICKEN) $$($(1)-RC_FILE)
	$$(LINKER) $$(LINKER_OPTIONS) $$(LINKER_EXECUTABLE_OPTIONS) $$< \
          $$($(1)-RC_FILE) $$(LINKER_OUTPUT) \
          $$(LINKER_LIBRARY_PREFIX)$$(PROGRAM_PREFIX)chicken$$(PROGRAM_SUFFIX)$$(LINKER_LIBRARY_SUFFIX) \
          $$(LINKER_LINK_SHARED_PROGRAM_OPTIONS) $$(LIBRARIES)
endef

# Unfortunately, we can't loop over INSTALLED_PROGRAMS here because of
# the possible name mangling and EXE suffixing in there :(
$(eval $(call declare-program-from-object,$(CSI_SHARED_EXECUTABLE),csi))
$(eval $(call declare-program-from-object,$(CHICKEN_INSTALL_PROGRAM)$(EXE),chicken-install,true))
$(eval $(call declare-program-from-object,$(CHICKEN_UNINSTALL_PROGRAM)$(EXE),chicken-uninstall,true))
$(eval $(call declare-program-from-object,$(CHICKEN_STATUS_PROGRAM)$(EXE),chicken-status))
$(eval $(call declare-program-from-object,$(CHICKEN_PROFILE_PROGRAM)$(EXE),chicken-profile))
$(eval $(call declare-program-from-object,$(CSC_PROGRAM)$(EXE),csc))

# static executables

$(CHICKEN_STATIC_EXECUTABLE): $(COMPILER_STATIC_OBJECTS) lib$(PROGRAM_PREFIX)chicken$(PROGRAM_SUFFIX)$(A)
	$(LINKER) $(LINKER_OPTIONS) $(LINKER_STATIC_OPTIONS) $(COMPILER_STATIC_OBJECTS) $(LINKER_OUTPUT) lib$(PROGRAM_PREFIX)chicken$(PROGRAM_SUFFIX)$(A) $(LIBRARIES)

define declare-static-program-from-object
$(1): $(2)$(O) lib$(PROGRAM_PREFIX)chicken$(PROGRAM_SUFFIX)$(A)
	$$(LINKER) $$(LINKER_OPTIONS) $$(LINKER_STATIC_OPTIONS) $$< $$(LINKER_OUTPUT) lib$(PROGRAM_PREFIX)chicken$(PROGRAM_SUFFIX)$(A) $$(LIBRARIES)
endef

$(eval $(call declare-program-from-object,$(CSI_STATIC_EXECUTABLE),csi))
$(eval $(call declare-program-from-object,$(CHICKEN_BUG_PROGRAM)$(EXE),chicken-bug))


# installation

.PHONY: install uninstall install-libs
.PHONY: install-target install-dev install-bin install-other-files install-wrappers

install: $(TARGETS) install-target install-bin install-libs install-dev install-other-files

install-target: install-libs

install-libs:
	$(MAKEDIR_COMMAND) $(MAKEDIR_COMMAND_OPTIONS) "$(DESTDIR)$(ILIBDIR)"
ifneq ($(LIBCHICKEN_IMPORT_LIBRARY),) 
	$(INSTALL_PROGRAM) $(INSTALL_PROGRAM_STATIC_LIBRARY_OPTIONS) $(LIBCHICKEN_IMPORT_LIBRARY) "$(DESTDIR)$(ILIBDIR)"
endif
ifndef STATICBUILD
ifdef DLLSINPATH
	$(MAKEDIR_COMMAND) $(MAKEDIR_COMMAND_OPTIONS) "$(DESTDIR)$(IBINDIR)"
	$(INSTALL_PROGRAM) $(INSTALL_PROGRAM_SHARED_LIBRARY_OPTIONS) $(LIBCHICKEN_SO_FILE) "$(DESTDIR)$(IBINDIR)"
else
	$(INSTALL_PROGRAM) $(INSTALL_PROGRAM_SHARED_LIBRARY_OPTIONS) $(LIBCHICKEN_SO_FILE) "$(DESTDIR)$(ILIBDIR)$(SEP)$(LIBCHICKEN_SO_FILE)$(SONAME_VERSION)"
endif
ifdef USES_SONAME
	cd "$(DESTDIR)$(ILIBDIR)" && ln -sf $(LIBCHICKEN_SO_FILE).$(BINARYVERSION) lib$(PROGRAM_PREFIX)chicken$(PROGRAM_SUFFIX)$(SO)
endif
endif

define NL


endef # A newline, used to inject recipe lines in a loop. Ugly, but necessary

install-dev: install-libs
	$(MAKEDIR_COMMAND) $(MAKEDIR_COMMAND_OPTIONS) "$(DESTDIR)$(ILIBDIR)"
	$(MAKEDIR_COMMAND) $(MAKEDIR_COMMAND_OPTIONS) "$(DESTDIR)$(ISHAREDIR)"
	$(MAKEDIR_COMMAND) $(MAKEDIR_COMMAND_OPTIONS) "$(DESTDIR)$(IEGGDIR)"
	$(MAKEDIR_COMMAND) $(MAKEDIR_COMMAND_OPTIONS) "$(DESTDIR)$(IINCDIR)"
	$(MAKEDIR_COMMAND) $(MAKEDIR_COMMAND_OPTIONS) "$(DESTDIR)$(IDATADIR)"
	$(INSTALL_PROGRAM) $(INSTALL_PROGRAM_STATIC_LIBRARY_OPTIONS) lib$(PROGRAM_PREFIX)chicken$(PROGRAM_SUFFIX)$(A) "$(DESTDIR)$(ILIBDIR)"
ifneq ($(POSTINSTALL_STATIC_LIBRARY),true)
	$(POSTINSTALL_STATIC_LIBRARY) $(POSTINSTALL_STATIC_LIBRARY_FLAGS) "$(ILIBDIR)$(SEP)libchicken$(A)"
endif
	$(INSTALL_PROGRAM) $(INSTALL_PROGRAM_FILE_OPTIONS) $(SRCDIR)chicken.h "$(DESTDIR)$(IINCDIR)"
	$(INSTALL_PROGRAM) $(INSTALL_PROGRAM_FILE_OPTIONS) $(CHICKEN_CONFIG_H) "$(DESTDIR)$(IINCDIR)"
ifeq ($(PLATFORM),macosx)
	$(INSTALL_PROGRAM) $(INSTALL_PROGRAM_FILE_OPTIONS) $(SRCDIR)mac.r "$(DESTDIR)$(ISHAREDIR)"
	$(INSTALL_PROGRAM) $(INSTALL_PROGRAM_FILE_OPTIONS) $(SRCDIR)CHICKEN.icns "$(DESTDIR)$(IDATADIR)"
endif
ifdef WINDOWS
	$(INSTALL_PROGRAM) $(INSTALL_PROGRAM_FILE_OPTIONS) $(SRCDIR)chicken.ico "$(DESTDIR)$(IDATADIR)"
	$(INSTALL_PROGRAM) $(INSTALL_PROGRAM_FILE_OPTIONS) $(SRCDIR)chicken.rc$(O) "$(DESTDIR)$(IDATADIR)"
endif
	$(INSTALL_PROGRAM) $(INSTALL_PROGRAM_FILE_OPTIONS) $(SRCDIR)types.db "$(DESTDIR)$(IEGGDIR)"

ifeq ($(NEEDS_RELINKING),yes)
install-bin:
	$(foreach prog,$(INSTALLED_PROGRAMS),\
		$(REMOVE_COMMAND) $(REMOVE_COMMAND_OPTIONS) $(prog)$(EXE) $(NL))

	$(REMOVE_COMMAND) $(REMOVE_COMMAND_OPTIONS) $(IMPORT_LIBRARIES:%=%.so)
	$(REMOVE_COMMAND) $(REMOVE_COMMAND_OPTIONS) $(IMPORT_LIBRARIES:%=%.import.so)
	$(REMOVE_COMMAND) $(REMOVE_COMMAND_OPTIONS) $(LIBCHICKEN_SO_FILE)
	$(MAKE) -f $(SRCDIR)Makefile.$(PLATFORM) CONFIG=$(CONFIG) NEEDS_RELINKING=no RUNTIME_LINKER_PATH="$(LIBDIR)" SONAME_VERSION=.$(BINARYVERSION) install-bin
# Damn. What was this for, again?
#
# 	$(MAKE_WRITABLE_COMMAND) $(CHICKEN_PROGRAM)$(EXE) $(CSI_PROGRAM)$(EXE) $(CSC_PROGRAM)$(EXE) $(CHICKEN_PROFILE_PROGRAM)$(EXE)
# ifndef STATICBUILD
# 	$(MAKE_WRITABLE_COMMAND) $(CHICKEN_INSTALL_PROGRAM)$(EXE)
# 	$(MAKE_WRITABLE_COMMAND) $(CHICKEN_UNINSTALL_PROGRAM)$(EXE)
# 	$(MAKE_WRITABLE_COMMAND) $(CHICKEN_STATUS_PROGRAM)$(EXE)
# endif
else
install-bin: $(TARGETS) install-libs install-dev
	$(MAKEDIR_COMMAND) $(MAKEDIR_COMMAND_OPTIONS) "$(DESTDIR)$(IBINDIR)"

	$(foreach prog,$(INSTALLED_PROGRAMS),\
		$(INSTALL_PROGRAM) $(INSTALL_PROGRAM_EXECUTABLE_OPTIONS) \
		$(prog)$(EXE) "$(DESTDIR)$(IBINDIR)" $(NL))

ifdef STATICBUILD
	$(foreach lib,$(IMPORT_LIBRARIES),\
		$(INSTALL_PROGRAM) $(INSTALL_PROGRAM_FILE_OPTIONS) \
		$(lib).import.scm "$(DESTDIR)$(IEGGDIR)" $(NL))
else
	$(foreach lib,$(IMPORT_LIBRARIES),\
		$(INSTALL_PROGRAM) $(INSTALL_PROGRAM_EXECUTABLE_OPTIONS) \
		$(lib).import.so "$(DESTDIR)$(IEGGDIR)" $(NL))
endif

# XXX Shouldn't this be part of the non-static lib part?
	$(foreach setup-lib,$(SETUP_API_OBJECTS_1),\
		$(INSTALL_PROGRAM) $(INSTALL_PROGRAM_EXECUTABLE_OPTIONS) \
		$(setup-lib).so "$(DESTDIR)$(IEGGDIR)" $(NL))

ifndef STATICBUILD
ifneq ($(POSTINSTALL_PROGRAM),true)
	$(foreach prog,$(INSTALLED_PROGRAMS),\
		$(POSTINSTALL_PROGRAM) $(POSTINSTALL_PROGRAM_FLAGS) \
		"$(DESTDIR)$(IBINDIR)$(SEP)$(prog)" $(NL))

	$(foreach apilib,$(SETUP_API_OBJECTS_1),\
		$(POSTINSTALL_PROGRAM) $(POSTINSTALL_PROGRAM_FLAGS) \
		"$(DESTDIR)$(IEGGDIR)$(SEP)$(apilib).so" $(NL))

	$(foreach import-lib,$(IMPORT_LIBRARIES),\
		$(POSTINSTALL_PROGRAM) $(POSTINSTALL_PROGRAM_FLAGS) \
		"$(DESTDIR)$(IEGGDIR)$(SEP)$(import-lib).import.so" $(NL))
endif
ifeq ($(CROSS_CHICKEN)$(DESTDIR),0)
	-$(IBINDIR)$(SEP)$(CHICKEN_INSTALL_PROGRAM) -update-db
else
	@echo
	@echo "Warning: cannot run \`$(CHICKEN_INSTALL_PROGRAM) -update-db' when cross-compiling or DESTDIR is set"
	@echo
endif
endif
ifdef WINDOWS_SHELL
	$(INSTALL_PROGRAM) $(INSTALL_PROGRAM_EXECUTABLE_OPTIONS) $(SRCDIR)csibatch.bat "$(DESTDIR)$(IBINDIR)"
endif
endif

install-other-files:
	$(MAKEDIR_COMMAND) $(MAKEDIR_COMMAND_OPTIONS) "$(DESTDIR)$(IMANDIR)"
	$(MAKEDIR_COMMAND) $(MAKEDIR_COMMAND_OPTIONS) "$(DESTDIR)$(IDOCDIR)"
	$(MAKEDIR_COMMAND) $(MAKEDIR_COMMAND_OPTIONS) "$(DESTDIR)$(IDATADIR)"
	$(foreach obj, $(MANPAGES), \
		$(INSTALL_PROGRAM) $(INSTALL_PROGRAM_FILE_OPTIONS) \
		$(SRCDIR)$(obj) "$(DESTDIR)$(IMANDIR)" $(NL))
	$(MAKEDIR_COMMAND) $(MAKEDIR_COMMAND_OPTIONS) "$(DESTDIR)$(IDOCDIR)$(SEP)manual"
	-$(INSTALL_PROGRAM) $(INSTALL_PROGRAM_FILE_OPTIONS) $(SRCDIR)manual-html$(SEP)* "$(DESTDIR)$(IDOCDIR)$(SEP)manual"
	$(INSTALL_PROGRAM) $(INSTALL_PROGRAM_FILE_OPTIONS) $(SRCDIR)README "$(DESTDIR)$(IDOCDIR)"
	$(INSTALL_PROGRAM) $(INSTALL_PROGRAM_FILE_OPTIONS) $(SRCDIR)LICENSE "$(DESTDIR)$(IDOCDIR)"
	$(INSTALL_PROGRAM) $(INSTALL_PROGRAM_FILE_OPTIONS) $(SRCDIR)setup.defaults "$(DESTDIR)$(IDATADIR)"

install-wrappers:
ifeq ($(WRAPPERDIR),)
	@echo 
	@echo Error: WRAPPERDIR is not set
	@echo
	@exit 1
endif
	$(foreach prg, $(INSTALLED_PROGRAMS), \
		$(CSI) -s $(SRCDIR)scripts$(SEP)make-wrapper.scm $(prg) "$(WRAPPERDIR)" $(NL))

uninstall:
	$(foreach prog,$(INSTALLED_PROGRAMS),\
		$(REMOVE_COMMAND) $(REMOVE_COMMAND_OPTIONS)\
		"$(DESTDIR)$(IBINDIR)$(SEP)$(prog)$(EXE)" $(NL))

	$(REMOVE_COMMAND) $(REMOVE_COMMAND_RECURSIVE_OPTIONS) "$(DESTDIR)$(IEGGDIR)"

	$(REMOVE_COMMAND) $(REMOVE_COMMAND_OPTIONS) "$(DESTDIR)$(ILIBDIR)$(SEP)lib$(PROGRAM_PREFIX)chicken$(PROGRAM_SUFFIX)$(A)"
	$(REMOVE_COMMAND) $(REMOVE_COMMAND_OPTIONS) "$(DESTDIR)$(ILIBDIR)$(SEP)lib$(PROGRAM_PREFIX)chicken$(PROGRAM_SUFFIX)$(SO)"
ifdef USES_SONAME
	-$(REMOVE_COMMAND) $(REMOVE_COMMAND_OPTIONS) "$(DESTDIR)$(ILIBDIR)$(SEP)lib$(PROGRAM_PREFIX)chicken$(PROGRAM_SUFFIX)$(SO).$(BINARYVERSION)"
endif
ifdef WINDOWS
	$(REMOVE_COMMAND) $(REMOVE_COMMAND_OPTIONS) "$(DESTDIR)$(IBINDIR)$(SEP)lib$(PROGRAM_PREFIX)chicken$(PROGRAM_SUFFIX)$(SO)"
	$(REMOVE_COMMAND) $(REMOVE_COMMAND_OPTIONS) "$(DESTDIR)$(ILIBDIR)$(SEP)$(LIBCHICKEN_IMPORT_LIBRARY)"
endif
ifeq ($(PLATFORM),cygwin)
	$(REMOVE_COMMAND) $(REMOVE_COMMAND_OPTIONS) "$(DESTDIR)$(IBINDIR)$(SEP)cyg$(PROGRAM_PREFIX)chicken$(PROGRAM_SUFFIX)*"
endif

	$(foreach obj,$(MANPAGES),\
		$(REMOVE_COMMAND) $(REMOVE_COMMAND_OPTIONS) \
		"$(DESTDIR)$(IMANDIR)$(SEP)$(obj)" $(NL))

	$(REMOVE_COMMAND) $(REMOVE_COMMAND_OPTIONS) "$(DESTDIR)$(IINCDIR)$(SEP)chicken.h"
	$(REMOVE_COMMAND) $(REMOVE_COMMAND_OPTIONS) "$(DESTDIR)$(IINCDIR)$(SEP)$(CHICKEN_CONFIG_H)"
	$(REMOVE_COMMAND) $(REMOVE_COMMAND_RECURSIVE_OPTIONS) "$(DESTDIR)$(IDATADIR)"
	$(REMOVE_COMMAND) $(REMOVE_COMMAND_RECURSIVE_OPTIONS) "$(DESTDIR)$(IEGGDIR)"
ifdef WINDOWS_SHELL
	$(REMOVE_COMMAND) $(REMOVE_COMMAND_OPTIONS) "$(DESTDIR)$(IBINDIR)$(SEP)csibatch.bat"
endif

# build versioning

ifdef WINDOWS_SHELL
buildtag.h:
	echo #define C_BUILD_TAG "$(BUILD_TAG)" >$@
buildbranch:
	echo.$(BRANCHNAME)>buildbranch
buildid:
	echo.$(BUILD_ID)>buildid
else
.PHONY: identify-me

identify-me:
	@sh $(SRCDIR)identify.sh $(SRCDIR)

buildtag.h: identify-me
buildbranch: identify-me
buildid: identify-me
endif

# bootstrapping c sources

define declare-emitted-import-lib-dependency
.SECONDARY: $(1).import.scm
$(1).import.scm: $(1).c
endef

$(foreach lib, $(SETUP_API_OBJECTS_1),\
          $(eval $(call declare-emitted-import-lib-dependency,$(lib))))

define profile-flags
$(if $(filter $(basename $(1)),$(PROFILE_OBJECTS)),-profile)
endef

bootstrap-lib = $(CHICKEN) $(call profile-flags, $@) $< $(CHICKEN_LIBRARY_OPTIONS) -output-file $@

library.c: $(SRCDIR)library.scm $(SRCDIR)banner.scm $(SRCDIR)common-declarations.scm
	$(bootstrap-lib)
eval.c: $(SRCDIR)eval.scm $(SRCDIR)common-declarations.scm
	$(bootstrap-lib)
expand.c: $(SRCDIR)expand.scm $(SRCDIR)synrules.scm $(SRCDIR)common-declarations.scm
	$(bootstrap-lib)
modules.c: $(SRCDIR)modules.scm $(SRCDIR)common-declarations.scm
	$(bootstrap-lib)
extras.c: $(SRCDIR)extras.scm $(SRCDIR)private-namespace.scm $(SRCDIR)common-declarations.scm
	$(bootstrap-lib)
posixunix.c: $(SRCDIR)posixunix.scm $(SRCDIR)posix-common.scm $(SRCDIR)common-declarations.scm
	$(bootstrap-lib) 
posixwin.c: $(SRCDIR)posixwin.scm $(SRCDIR)posix-common.scm $(SRCDIR)common-declarations.scm
	$(bootstrap-lib) 
srfi-69.c: $(SRCDIR)srfi-69.scm $(SRCDIR)common-declarations.scm
	$(bootstrap-lib) -extend $(SRCDIR)private-namespace.scm
irregex.c: $(SRCDIR)irregex.scm $(SRCDIR)irregex-core.scm $(SRCDIR)irregex-utils.scm $(SRCDIR)common-declarations.scm
	$(bootstrap-lib)
#
# The ones below just depend on their matching .scm file and common-declarations
#
chicken-syntax.c: $(SRCDIR)chicken-syntax.scm $(SRCDIR)common-declarations.scm
	$(bootstrap-lib)
chicken-ffi-syntax.c: $(SRCDIR)chicken-ffi-syntax.scm $(SRCDIR)common-declarations.scm
	$(bootstrap-lib)
data-structures.c: $(SRCDIR)data-structures.scm $(SRCDIR)common-declarations.scm
	$(bootstrap-lib)
ports.c: $(SRCDIR)ports.scm $(SRCDIR)common-declarations.scm
	$(bootstrap-lib)
files.c: $(SRCDIR)files.scm $(SRCDIR)common-declarations.scm
	$(bootstrap-lib)
lolevel.c: $(SRCDIR)lolevel.scm $(SRCDIR)common-declarations.scm
	$(bootstrap-lib) 
tcp.c: $(SRCDIR)tcp.scm $(SRCDIR)common-declarations.scm
	$(bootstrap-lib) 
srfi-1.c: $(SRCDIR)srfi-1.scm $(SRCDIR)common-declarations.scm
	$(bootstrap-lib) 
srfi-4.c: $(SRCDIR)srfi-4.scm $(SRCDIR)common-declarations.scm
	$(bootstrap-lib) 
srfi-13.c: $(SRCDIR)srfi-13.scm $(SRCDIR)common-declarations.scm
	$(bootstrap-lib) 
srfi-14.c: $(SRCDIR)srfi-14.scm $(SRCDIR)common-declarations.scm
	$(bootstrap-lib) 
srfi-18.c: $(SRCDIR)srfi-18.scm $(SRCDIR)common-declarations.scm
	$(bootstrap-lib) 
utils.c: $(SRCDIR)utils.scm $(SRCDIR)common-declarations.scm
	$(bootstrap-lib) 
scheduler.c: $(SRCDIR)scheduler.scm $(SRCDIR)common-declarations.scm
	$(bootstrap-lib) 
profiler.c: $(SRCDIR)profiler.scm $(SRCDIR)common-declarations.scm
	$(bootstrap-lib) 
stub.c: $(SRCDIR)stub.scm $(SRCDIR)common-declarations.scm
	$(bootstrap-lib) 
build-version.c: $(SRCDIR)build-version.scm buildbranch buildid \
	  $(SRCDIR)buildversion buildtag.h
	$(bootstrap-lib)

define declare-bootstrap-import-lib
$(1).import.c: $$(SRCDIR)$(1).import.scm
	$$(CHICKEN) $$< $$(CHICKEN_IMPORT_LIBRARY_OPTIONS) -output-file $$@
endef

$(foreach obj, $(IMPORT_LIBRARIES),\
          $(eval $(call declare-bootstrap-import-lib,$(obj))))

# Bootstrap compiler objects

define declare-bootstrap-compiler-object
$(1).c: $$(SRCDIR)$(1).scm $$(SRCDIR)compiler-namespace.scm \
	  $$(SRCDIR)private-namespace.scm $$(SRCDIR)tweaks.scm
	$$(CHICKEN) $$< $$(CHICKEN_COMPILER_OPTIONS) -output-file $$@ 
endef

$(foreach obj, $(COMPILER_OBJECTS_1),\
          $(eval $(call declare-bootstrap-compiler-object,$(obj))))

csi.c: $(SRCDIR)csi.scm $(SRCDIR)banner.scm $(SRCDIR)private-namespace.scm
	$(CHICKEN) $< $(CHICKEN_PROGRAM_OPTIONS) -output-file $@ -extend $(SRCDIR)private-namespace.scm
chicken-profile.c: $(SRCDIR)chicken-profile.scm
	$(CHICKEN) $< $(CHICKEN_PROGRAM_OPTIONS) -output-file $@ 
chicken-install.c: $(SRCDIR)chicken-install.scm setup-download.c setup-api.c
	$(CHICKEN) $< $(CHICKEN_PROGRAM_OPTIONS) -output-file $@ 
chicken-uninstall.c: $(SRCDIR)chicken-uninstall.scm
	$(CHICKEN) $< $(CHICKEN_PROGRAM_OPTIONS) -output-file $@ 
chicken-status.c: $(SRCDIR)chicken-status.scm
	$(CHICKEN) $< $(CHICKEN_PROGRAM_OPTIONS) -output-file $@ 
csc.c: $(SRCDIR)csc.scm
	$(CHICKEN) $< $(CHICKEN_PROGRAM_OPTIONS) -output-file $@ 
chicken-bug.c: $(SRCDIR)chicken-bug.scm
	$(CHICKEN) $< $(CHICKEN_PROGRAM_OPTIONS) -output-file $@ 

setup-api.c: $(SRCDIR)setup-api.scm
	$(CHICKEN) $< $(CHICKEN_DYNAMIC_OPTIONS) -emit-import-library setup-api \
	  -output-file $@ 
setup-download.c: $(SRCDIR)setup-download.scm setup-api.c
	$(CHICKEN) $< $(CHICKEN_DYNAMIC_OPTIONS) -emit-import-library setup-download \
	  -output-file $@ 

# distribution files

.PHONY: distfiles dist html

distfiles: $(DISTFILES)

dist: distfiles html
	CSI=$(CSI) $(CSI) -s $(SRCDIR)scripts$(SEP)makedist.scm -platform $(PLATFORM) CHICKEN=$(CHICKEN)

# Jim's `manual-labor' must be installed (just run "chicken-install manual-labor")
html:
	$(MAKEDIR_COMMAND) $(MAKEDIR_COMMAND_OPTIONS) $(SRCDIR)manual-html
	manual-labor $(SRCDIR)manual $(SRCDIR)manual-html
	$(COPY_COMMAND) $(SRCDIR)chicken.png manual-html
	$(COPY_COMMAND) $(SRCDIR)manual.css manual-html
	$(COPY_COMMAND) $(SRCDIR)index.html manual-html

# cleaning up

.PHONY: clean distclean spotless confclean testclean

clean:
	-$(REMOVE_COMMAND) $(REMOVE_COMMAND_OPTIONS) $(PROGRAM_PREFIX)chicken$(PROGRAM_SUFFIX)$(EXE) $(PROGRAM_PREFIX)csi$(PROGRAM_SUFFIX)$(EXE) $(PROGRAM_PREFIX)csc$(PROGRAM_SUFFIX)$(EXE) \
	  $(CHICKEN_PROFILE_PROGRAM)$(EXE) \
	  $(CHICKEN_INSTALL_PROGRAM)$(EXE) \
	  $(CHICKEN_UNINSTALL_PROGRAM)$(EXE) \
	  $(CHICKEN_STATUS_PROGRAM)$(EXE) \
	  $(CHICKEN_BUG_PROGRAM)$(EXE) *$(O) \
	  $(LIBCHICKEN_SO_FILE) \
	  $(PRIMARY_LIBCHICKEN) \
	  lib$(PROGRAM_PREFIX)chicken$(PROGRAM_SUFFIX)$(A) \
	  $(PROGRAM_IMPORT_LIBRARIES) \
	  $(IMPORT_LIBRARIES:=.import.so) $(LIBCHICKEN_IMPORT_LIBRARY) \
	  $(SETUP_API_OBJECTS_1:=.so) $(SETUP_API_OBJECTS_1:=.import.so)
ifdef USES_SONAME
	$(REMOVE_COMMAND) $(REMOVE_COMMAND_OPTIONS) lib$(PROGRAM_PREFIX)chicken$(PROGRAM_SUFFIX).so.$(BINARYVERSION)
endif

confclean:
	-$(REMOVE_COMMAND) $(REMOVE_COMMAND_OPTIONS) \
	  chicken-config.h chicken-defaults.h chicken-install.rc chicken-uninstall.rc

spotless: distclean testclean
	-$(REMOVE_COMMAND) $(REMOVE_COMMAND_OPTIONS) $(DISTFILES) \
	  buildtag.h buildid buildbranch


distclean: clean confclean

testclean:
	$(REMOVE_COMMAND) $(REMOVE_COMMAND_RECURSIVE_OPTIONS) $(SRCDIR)tests$(SEP)a.out $(SRCDIR)tests$(SEP)scrutiny.out \
	  $(SRCDIR)tests$(SEP)tmp* $(SRCDIR)tests$(SEP)*.so $(SRCDIR)tests$(SEP)*.import.scm $(SRCDIR)tests$(SEP)repository $(SRCDIR)tests$(SEP)*.dll

# run tests

.PHONY: check 

check: $(CHICKEN_SHARED_EXECUTABLE) $(CSI_SHARED_EXECUTABLE) $(CSC_PROGRAM)$(EXE)
ifndef WINDOWS_SHELL
	cd tests; sh runtests.sh
else
	cd tests & runtests.bat
endif

# benchmark

.PHONY: bench

bench: $(CHICKEN_SHARED_EXECUTABLE) $(CSI_SHARED_EXECUTABLE) $(CSC_PROGRAM)$(EXE)
	cd tests; echo >>bench.log; date >>bench.log; sh runbench.sh 2>&1 | tee -a bench.log


# build static bootstrapping chicken

.PHONY: boot-chicken

boot-chicken:
	$(MAKE) -f Makefile.$(PLATFORM) PLATFORM=$(PLATFORM) PREFIX=/nowhere CONFIG= \
	  CHICKEN=$(CHICKEN) PROGRAM_SUFFIX=-boot-stage1 STATICBUILD=1 \
	  C_COMPILER_OPTIMIZATION_OPTIONS= C_HACKED_APPLY= BUILDING_CHICKEN_BOOT=1 \
	  confclean chicken-boot-stage1$(EXE)
	$(MAKE) -f Makefile.$(PLATFORM) PLATFORM=$(PLATFORM) PREFIX=/nowhere CONFIG= \
	  CHICKEN=.$(SEP)chicken-boot-stage1$(EXE) PROGRAM_SUFFIX=-boot \
	  STATICBUILD=1 C_COMPILER_OPTIMIZATION_OPTIONS= \
	  touchfiles chicken-boot$(EXE) confclean

.PHONY: touchfiles

touchfiles:
ifdef WINDOWS_SHELL
	for %%x in (*.scm) do copy /b %%x +,,
else
	touch *.scm
endif
