# Flags
DIR := libmojave/stdlib
ROOT := ../..

include $(ROOT)/mk/preface

INCLUDE :=

# Library files
include Files
MLZFILES := $(STDLIB_MLZFILES)
LMFILES := $(STDLIB_FILES)
LMIFILES := $(STDLIB_FILES)

# Name of library
MAIN := lmlib

INSTALL_LIBS := $(MAIN)

.PHONY: lib

all: lib

#
# Actual makefile
#
include $(ROOT)/mk/rules
include Makefile.dep

#
# This is a hack.
#
lm_pervasives.ml: lm_pervasives.mlp lm_pervasives.h
	$(CPP) -imacros lm_pervasives.h lm_pervasives.mlp > $@

ifeq ($(THREADS_ENABLED),YES)

lm_threads.ml: lm_threads_system.ml
	$(LN) $< $@
else

lm_threads.ml: lm_threads_null.ml
	$(LN) $< $@

endif

clean::
	$(RM) lm_threads.ml

#
# Rules
#
lib: $(RMLIFILES) $(RCMIFILES) $(RCMAFILES)

opt: $(RMLIFILES) $(RCMIFILES) $(RCMXAFILES)
	@ $(MAKE) $(RAFILES)

install:: $(MAIN).cma $(CMIFILES)
	$(INSTALL) $(MLIFILES) $(CMIFILES) $(MPLIB)

Makefile.dep: Files

