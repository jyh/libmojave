# Flags
DIR := libmojave/unix
ROOT := ../..

include $(ROOT)/mk/preface

INCLUDE :=

# Library files
include Files
LMFILES := $(UNIX_FILES)
LMIFILES := $(UNIX_FILES)

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
