# GHC-specific and GNU-make-specific makefile for the `parr' library
#
# Copyright (c) [2001..2002] Manuel M T Chakravarty & Gabriele Keller
#
# $Id: Makefile,v 1.24 2002/12/02 07:42:39 chak Exp $
#
# This file may be used, modified, and distributed under the same conditions
# and the same warranty disclaimer as set out in the X11 license.
#
# Description ---------------------------------------------------------------
#
# Language: GNU make specification
#
# Simple makefile that compiles the `parr' library into a GHC package
#
# Todo ----------------------------------------------------------------------
#
# * Generate dependencies automatically
#

# Configuration options
# ---------------------

# Executable containing GHC 6.5 (upwards)
GHC     = ../../../../../../ghc/compiler/ghc-inplace

# Executable containing the package manager for GHC
GHC_PKG = ghc-pkg

# Installation prefix (where "make install" puts the library)
PREFIX  = /usr/local

# GHC package config file where "make install" registers the library (if it is
# empty, the library is registered in GHC's main package config file; make sure
# you have write permissions to that file)
PKGCONF =

# Compile and install a debugging version of the library (must be "True" or 
# "False")
DEBUG = False

# Tools (needs GNU tar and GNU ld)
#
TAR     = tar
LN	= ln
GREP	= grep
SED	= sed
INSTALL = install
AR	= ar
LD	= ld

# ---------------- For a normal installation, you shouldn't -----------------
# ---------------- need to change anything past this point  -----------------

# Haskell compiler
#
HC      = $(GHC)
HCFLAGS = -i../../../..\
          -fglasgow-exts -O2 -funbox-strict-fields\
	  -fliberate-case-threshold100 -fno-method-sharing
#	  -fno-cse
# NB: method sharing and CSE can hinder fusion
HLDFLAGS= -L. -l$(LIB)

# Install info
#
PACKAGE    = parr
ifeq ($(DEBUG),False)
PKGNAME    = $(PACKAGE)
else
PKGNAME    = $(PACKAGE)-debug
endif
LIB_DIR    = $(PREFIX)/lib/$(PKGNAME)-$(VERSION)
IMPORT_DIR = $(LIB_DIR)/import
CONF	   = parr.conf

# Files
#
SRCS    = ../Base/Config.hs ../Base/Debug.hs\
          ../Base/BUArr.hs ../Base/BBArr.hs\
	  ../Base/Generics.hs ../Base/Hyperstrict.hs ../Base/Prim.hs\
	  UArr.hs\
	  ../Declarative/Loop.hs ../Declarative/Fusion.hs\
	  ../Unlifted/ListLike.hs ../Unlifted/NeslLike.hs ../Unlifted.hs
OBJS    = $(patsubst %.hs,%.o,$(filter %.hs,$(SRCS)))
LIB     = parrHS

TARFILES = README TODO Makefile $(SRCS)\
	   test/simple/*.hs test/sumsq/*.hs test/dotp/*.hs test/primes/*.hs\
	   test/smvm/*.hs test/unit/*.hs\
	   test/benchutils/*.hs

# files that contain a `versnum = "x.y.z"' line
#
VERSFILE=../Unlifted.hs

# this is far from elegant, but extracts the plain version number
#
VERSION =$(shell $(GREP) '^versnum' $(VERSFILE)\
		 | $(SED) '-e s/versnum.* "//' '-e s/"//')

# Base directory for tar balls and exclude patterns
#
TARBASE=$(PACKAGE)
TAREXCL=--exclude='*/CVS*' --exclude='*~'

# Default target
#
.PHONY: default
default: all

# Debug config
#
.PHONY: PADebug.hs-build
PADebug.hs-build:
#ifneq ($(DEBUG),$(subst debug = ,,$(shell $(GREP) 'debug =' PADebug.hs)))
#	echo "-- Generated file -- DO NOT EDIT! --"	 >PADebug.hs
#	echo "module PADebug (debug) where"		>>PADebug.hs
#	echo "debug = $(DEBUG)"				>>PADebug.hs
#endif

# Templates
#
%.o : %.hs
	$(HC) -c $< $(HCFLAGS) $($(join $<,-HCFLAGS))
%.hi: %.o
	@:

# Main targets
#

.PHONY: all
all: lib$(LIB).a $(LIB).o

# library archive for static linking
lib$(LIB).a: $(OBJS)
	-$(RM) $@
	$(AR) -crs $@ $(OBJS)

# library object for GHCi
$(LIB).o: lib$(LIB).a
	$(LD) -r -x -o $@ --whole-archive $<

# install target
.PHONY: install
install: 
	$(INSTALL) -d $(LIB_DIR)
	$(INSTALL) lib$(LIB).a $(LIB_DIR)
	$(INSTALL) $(LIB).o $(LIB_DIR)
	$(INSTALL) -d $(IMPORT_DIR)
	$(INSTALL) $(patsubst %.hs,%.hi,$(filter %.hs,$(SRCS))) $(IMPORT_DIR)
	@echo "Package {"					 >$(CONF)
	@echo "  name = \"$(PKGNAME)\","			>>$(CONF)
	@echo "  import_dirs = [\"$(IMPORT_DIR)\"],"		>>$(CONF)
	@echo "  library_dirs = [\"$(LIB_DIR)\"],"		>>$(CONF)
	@echo "  hs_libraries = [\"$(LIB)\"]",			>>$(CONF)
	@echo "  extra_ghc_opts = [\"-fglasgow-exts\"]"		>>$(CONF)
	@echo "}"						>>$(CONF)
	@if [ ! -e $(PKGCONF) ]; then echo "[]" >$(PKGCONF); fi
	$(GHC_PKG) -u -i $(CONF) $(if $(PKGCONF),-f) $(PKGCONF)
	@$(RM) $(CONF)

# tar a distribution
TARCMD=$(TAR) -c -z $(TAREXCL) -h -f
.PHONY: tar
tar:
	-$(LN) -s . $(TARBASE)-$(VERSION)
	$(TARCMD) $(TARBASE)-$(VERSION).tar.gz\
	  $(addprefix $(TARBASE)-$(VERSION)/,$(TARFILES))
	$(RM) $(TARBASE)-$(VERSION)

# Benchmark utilities in test/benchutils/
#
test/benchutils/BenchUtils.hs-HCFLAGS =

BENCHOBJS =test/benchutils/BenchUtils.o
BENCHFLAGS=-itest/benchutils 

# Unit tests in test/unit/
#
TEST_UNIT_HCFLAGS=-ddump-simpl -no-recomp

test/unit/TestUArr.hs-HCFLAGS  = $(TEST_UNIT_HCFLAGS)
test/unit/TestBUArr.hs-HCFLAGS = $(TEST_UNIT_HCFLAGS)
test/unit/TestLoop.hs-HCFLAGS  = $(TEST_UNIT_HCFLAGS)

test/unit/TestUArr.o : UArr.o
test/unit/TestBUArr.o: UArr.o
test/unit/TestLoop.o : ../Declarative/Loop.o

BASEOBJS=../Base/Config.o ../Base/Debug.o\
         ../Base/Hyperstrict.o ../Base/Generics.o ../Base/Prim.o\
         ../Base/BUArr.o ../Base/BBArr.o

test/unit/TestBUArr: test/unit/TestBUArr.o $(BASEOBJS)
	$(HC) -o $@ $(HCFLAGS) $^
test/unit/TestUArr : test/unit/TestUArr.o UArr.o $(BASEOBJS)
	$(HC) -o $@ $(HCFLAGS) $^
test/unit/TestLoop : test/unit/TestLoop.o UArr.o ../Declarative/Loop.o\
		     $(BASEOBJS)
	$(HC) -o $@ $(HCFLAGS) $^

# Simple tests in test/simple/
#
TEST_SIMPLE_HCFLAGS=-ddump-simpl -no-recomp

test/simple/MapInc.hs-HCFLAGS      =$(TEST_SIMPLE_HCFLAGS)
test/simple/Sum.hs-HCFLAGS         =$(TEST_SIMPLE_HCFLAGS)
test/simple/PrefixSum.hs-HCFLAGS   =$(TEST_SIMPLE_HCFLAGS)
test/simple/SegSum.hs-HCFLAGS      =$(TEST_SIMPLE_HCFLAGS)
test/simple/SegPrefixSum.hs-HCFLAGS=$(TEST_SIMPLE_HCFLAGS)
test/simple/DotProd.hs-HCFLAGS     =$(TEST_SIMPLE_HCFLAGS)

test/simple/MapInc.o      : lib$(LIB).a
test/simple/Sum.o         : lib$(LIB).a
test/simple/PrefixSum.o   : lib$(LIB).a
test/simple/SegSum.o      : lib$(LIB).a
test/simple/SegPrefixSum.o: lib$(LIB).a
test/simple/DotProd.o     : lib$(LIB).a

# Simple sum square fusion test in test/sumsq/
#
TEST_SUMSQ_HCFLAGS=-ddump-simpl -ddump-simpl-stats -no-recomp

test/sumsq/SumSq.hs-HCFLAGS = $(TEST_SUMSQ_HCFLAGS)

test/sumsq/SumSq.o: lib$(LIB).a

test/sumsq/sumsq: test/sumsq/SumSq.o
	$(HC) -o $@ $(HCFLAGS) $^ $(HLDFLAGS)

# Simple dot product test in test/dotp/
#
TEST_DOTP_HCFLAGS=$(BENCHFLAGS) -ddump-simpl -ddump-simpl-stats

test/dotp/DotP.hs-HCFLAGS = $(TEST_DOTP_HCFLAGS)

test/dotp/DotP.o: lib$(LIB).a $(BENCHOBJS)

test/dotp/dotp: test/dotp/DotP.o $(BENCHOBJS)
	$(HC) -o $@ $(HCFLAGS) $^ $(HLDFLAGS)

# Prime sieve in test/primes/
#
TEST_PRIMES_HCFLAGS=$(BENCHFLAGS) -ddump-simpl -ddump-simpl-stats

test/primes/Primes.hs-HCFLAGS = $(TEST_PRIMES_HCFLAGS)

test/primes/Primes.o: lib$(LIB).a $(BENCHOBJS)

test/primes/primes: test/primes/Primes.o $(BENCHOBJS)
	$(HC) -o $@ $(HCFLAGS) $^ $(HLDFLAGS)

# Sparse matrix vector multiplication in test/smvm/
#
TEST_SMVM_HCFLAGS=$(BENCHFLAGS) -ddump-simpl-stats -no-recomp
TEST_SMVM_OBJS   =test/smvm/SMVM_array_compr.o test/smvm/SMVM_array_rec.o\
		  test/smvm/SMVM_fusion.o test/smvm/SMVM_optimal.o\
		  test/smvm/Main.o

test/smvm/SMVM_array_compr.hs-HCFLAGS=$(TEST_SMVM_HCFLAGS)
test/smvm/SMVM_array_rec.hs-HCFLAGS  =$(TEST_SMVM_HCFLAGS)
test/smvm/SMVM_fusion.hs-HCFLAGS     =$(TEST_SMVM_HCFLAGS)
test/smvm/SMVM_optimal.hs-HCFLAGS    =$(TEST_SMVM_HCFLAGS)
test/smvm/Main.hs-HCFLAGS            =$(BENCHFLAGS) -itest/smvm

test/smvm/SMVM_array_compr.o: lib$(LIB).a $(BENCHOBJS)
test/smvm/SMVM_array_rec.o  : lib$(LIB).a $(BENCHOBJS)
test/smvm/SMVM_fusion.o     : lib$(LIB).a $(BENCHOBJS)
test/smvm/SMVM_optimal.o    : lib$(LIB).a $(BENCHOBJS)
test/smvm/Main.o            : lib$(LIB).a $(BENCHOBJS)

test/smvm/Main.o: test/smvm/SMVM_array_compr.hi\
		  test/smvm/SMVM_array_rec.hi\
		  test/smvm/SMVM_fusion.hi\
		  test/smvm/SMVM_optimal.hi

test/smvm/smvm: $(TEST_SMVM_OBJS) $(BENCHOBJS)
	$(HC) -o $@ $(HCFLAGS) $^ $(HLDFLAGS)

# Auxilliary targets
#

.PHONY: clean
clean:
	-$(RM) *.hi *.o ../Base/*.hi ../Base/*.o\
	  test/unit/*.hi test/unit/*.o test/unit/TestUArr test/unit/TestBUArr\
	  test/simple/*.hi test/simple/*.o\
	  test/benchutils/*.hi test/benchutils/*.o\
	  test/dotp/*.hi test/dotp/*.o\
	  test/primes/*.hi test/primes/*.o\
	  test/smvm/*.hi test/smvm/*.o\
	  lib$(LIB).a $(LIB).o

# Dependencies
#
# Base/
../Base/Config.o     : ../Base/Config.hs
../Base/Debug.o      : ../Base/Debug.hs ../Base/Config.hi
../Base/Prim.o       : ../Base/Prim.hs
../Base/Hyperstrict.o: ../Base/Hyperstrict.hs
../Base/Generics.o   : ../Base/Generics.hs ../Base/Hyperstrict.hi
../Base/BUArr.o      : ../Base/BUArr.hs ../Base/Debug.hi
../Base/BBArr.o      : ../Base/BBArr.hs ../Base/Debug.hi
# Monadic/
UArr.o    : UArr.hs ../Base/BUArr.hi ../Base/Generics.hi ../Base/Prim.hi
# Declarative/
../Declarative/Loop.o  : ../Declarative/Loop.hs UArr.hi
../Declarative/Fusion.o: ../Declarative/Fusion.hs ../Declarative/Loop.hi\
			 UArr.hi
# Unlifted/
../Unlifted/ListLike.o: ../Unlifted/ListLike.hs ../Declarative/Fusion.hi\
                        ../Declarative/Loop.hi UArr.hi
../Unlifted/NeslLike.o: ../Unlifted/NeslLike.hs ../Unlifted/ListLike.hi\
			../Declarative/Fusion.hi ../Declarative/Loop.hi UArr.hi
# Public interface
../Unlifted.o: ../Unlifted.hs ../Unlifted/ListLike.hi ../Unlifted/NeslLike.hi\
	       UArr.hi
