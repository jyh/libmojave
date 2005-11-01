# Flags
DIR := libmojave/stdlib
ROOT := ../..

include $(ROOT)/mk/preface

INCLUDE :=

# Library files
include Files
MLZFILES := $(SYSTEM_MLZFILES)
LMFILES := $(SYSTEM_FILES)
LMIFILES := $(SYSTEM_FILES)

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
# Rules
#
lib: $(RMLIFILES) $(RCMIFILES) $(RCMAFILES)

opt: $(RMLIFILES) $(RCMIFILES) $(RCMXAFILES)
	@ $(MAKE) $(RAFILES)

install:: $(MAIN).cma $(CMIFILES)
	$(INSTALL) $(MLIFILES) $(CMIFILES) $(MPLIB)

Makefile.dep: Files

