#
# Refiner parts are in subdirectories
#
DIR := libmojave
ROOT := ..

include $(ROOT)/mk/preface

INCLUDE :=

DIRS :=\
	stdlib\
	util

ADIRS :=\
	cutil\
	$(DIRS)

IDIRS := $(addprefix -I , $(DIRS))

CMALIBS := $(addsuffix /lmlib.cma, $(DIRS))
CMXALIBS := $(addsuffix /lmlib.cmxa, $(DIRS))

MAINLIB := lm

INSTALL_LIBS := $(MAINLIB)

.PHONY: lib optlib

all: lib
	+ @ $(MAKE) $(ROOT)/lib/$(MAINLIB).cma

opt: optlib
	+ @ $(MAKE) $(ROOT)/lib/$(MAINLIB).cmxa
	+ @ $(MAKE) $(ROOT)/lib/$(MAINLIB)$(EXT_LIB)

#
# The real Makefile
#
include $(ROOT)/mk/rules

#
# Sub-dependencies
#
include util/Files
include stdlib/Files

#
# Optimization
#
NAML_STDLIB_CMXFILES := $(addprefix stdlib/, $(addsuffix .cmx, $(NAML_STDLIB_FILES)))
UTIL_CMXFILES        := $(addprefix util/,   $(addsuffix .cmx, $(UTIL_FILES)))

CMXFILES :=\
	$(NAML_STDLIB_CMXFILES)\
	$(UTIL_CMXFILES)

#
# Libraries
#
$(MAINLIB).cma: $(CMALIBS)
	$(OCAMLC) $(OCAMLCFLAGS) -a -o $(MAINLIB).cma $(CMALIBS)

$(MAINLIB)$(EXT_LIB): $(MAINLIB).cmxa

$(MAINLIB).cmxa: $(CMXFILES)
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -a -o $(MAINLIB).cmxa $(CMXFILES)

#
# Standard build rules
#
lib:
	+ @for i in $(ADIRS); do\
		if (echo Making lm_libmojave/$$i...; $(MAKE) -C $$i $@); then true; else exit 1; fi;\
	done

optlib:
	+ @for i in $(ADIRS); do\
		if (echo Making lm_libmojave/$$i...; $(MAKE) -C $$i opt); then true; else exit 1; fi;\
	done

install::
	+ @for i in $(ADIRS); do\
		if (echo Making lm_libmojave/$$i...; $(MAKE) -C $$i $@); then true; else exit 1; fi;\
	done
	$(INSTALL) $(MAINLIB).cma $(INSTALLLIB)

clean::
	+ @for i in $(ADIRS); do\
		if (echo Cleaning lm_libmojave/$$i...; $(MAKE) -C $$i $@); then true; else exit 1; fi;\
	done

depend::
	@$(RM) Makefile.dep
	@for i in $(ADIRS); do\
		if (echo Making lm_libmojave/$$i...; cd $$i && $(RM) Makefile.dep); then true; else exit 1; fi;\
	done

