# Include directories
ROOT := ../..
DIR := libmojave/cutil

include $(ROOT)/mk/preface

LCFILES :=\
	lm_abstract\
	lm_float\
	lm_ncurses\
	lm_printf\
	lm_raw\
	lm_mmap\
	lm_readline\
	lm_rusage\
	lm_terminfo\
	lm_marshal_shared

# Name of library
MAIN := liblmcutil

all: $(ROOT)/lib/$(MAIN)-byte$(EXT_LIB)

opt: $(ROOT)/lib/$(MAIN)-native$(EXT_LIB)

install:: $(MAIN)-byte$(EXT_LIB) $(MAIN)-native$(EXT_LIB)
	$(INSTALL) $(MAIN)-byte$(EXT_LIB) $(MPLIB)
	$(INSTALL) $(MAIN)-native$(EXT_LIB) $(MPLIB)

clean::
	$(RM) *-byte.c *-native.c

#
# Installation
#
$(ROOT)/lib/$(MAIN)-byte$(EXT_LIB): $(MAIN)-byte$(EXT_LIB)
	$(LN) ../$(DIR)/$(MAIN)-byte$(EXT_LIB) $@

$(ROOT)/lib/$(MAIN)-native$(EXT_LIB): $(MAIN)-native$(EXT_LIB)
	$(LN) ../$(DIR)/$(MAIN)-native$(EXT_LIB) $@

# Actual makefile
include $(ROOT)/mk/rules

CFLAGS := $(CFLAGS)\
	 -DREADLINE\
	 -DRL_COMPLETION_MATCHES=rl_completion_matches\
         -DRL_CP_TYPE=rl_compentry_func_t\
         -DRL_CPP_TYPE=rl_completion_func_t

include Makefile.dep
