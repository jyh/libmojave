# Flags
DIR  := libmojave/util
ROOT := ../..

include $(ROOT)/mk/preface

INCLUDE := -I ../stdlib

# Library files
include Files

LMFILES := $(UTIL_FILES)
LMIFILES := $(UTIL_FILES)

# Name of library
MAIN := lmlib

INSTALL_LIBS := $(MAIN)

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

