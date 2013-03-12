# -*- Makefile -*-

MAIN_SOURCES = local_tactic.ml

VERBOSEMAKE=yes
FRAMAC_SHARE =$(shell frama-c -print-path )
FRAMAC_LIBDIR =$(shell frama-c -print-libpath )
PLUGIN_NAME = LocalTactics
PLUGIN_CMO = $(MAIN_SOURCES:.ml=)

include $(FRAMAC_SHARE)/Makefile.dynamic

