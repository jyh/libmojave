# Flags
DIR := libmojave/stdlib
ROOT := ../..

include $(ROOT)/mk/preface

INCLUDE :=

# Library files
include Files
LMFILES := $(STDLIB_FILES)
LMIFILES := $(STDLIB_FILES)

# Name of library
MAIN := lmstdlib

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

