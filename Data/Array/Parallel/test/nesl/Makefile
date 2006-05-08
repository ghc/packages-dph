# Package ndp tests and benchmarks
#
# Copyright (c) 2006 Manuel M T Chakravarty
#
# This file may be used, modified, and distributed under the same conditions
# and the same warranty disclaimer as set out in the X11 license.
#

# Configuration options
# ---------------------

# Location of inplace GHC
GHC = ../../../../../../../compiler/ghc-inplace

# Location of NDP lib (to trigger rebuilds)
LIB = ../../../../../libHSndp.a

# Haskell compiler
#
HC      = $(GHC)
HCFLAGS = -package ndp\
          -fglasgow-exts -O2 -funbox-strict-fields\
	  -fliberate-case-threshold100 -fno-method-sharing
#	  -fno-cse
# NB: method sharing and CSE can hinder fusion

# Default target
#
.PHONY: default
default: 
	echo "Error: You need to build specific tests"

%.o: %.hs $(LIB)
	$(HC) -c $< $(HCFLAGS) -no-recomp
%hi: %.o
	@:

%: %.o Bench.o
	$(HC) -o $@ $< Bench.o $(HCFLAGS)

