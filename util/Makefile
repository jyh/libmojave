# Flags
DIR  := libmojave/util
ROOT := ../..

include $(ROOT)/mk/preface

INCLUDE := -I $(ROOT)/lm_libmojave/naml/stdlib

# Library files
include Files
LMFILES := $(UTIL_FILES)
LMIFILES := $(UTIL_FILES)

# Name of library
MAIN := lmutil

all: lib

#
# Actual makefile
#
include $(ROOT)/mk/rules
include Makefile.dep

#
# Rules
#
lib: $(MAIN).cma $(RMLIFLES) $(RCMIFILES) Files
opt: $(MAIN).cmxa $(RMLIFILES) $(RCMIFILES) Files

install:: $(MAIN).cma $(CMIFILES)
	$(INSTALL) $(MLIFILES) $(CMIFILES) $(MPLIB)

Makefile.dep: Files

