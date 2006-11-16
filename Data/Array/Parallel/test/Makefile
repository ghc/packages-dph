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
GHC = ../../../../../../compiler/ghc-inplace

# Location of NDP lib (to trigger rebuilds)
LIB = ../../../../libHSndp.a

# Haskell compiler
#
HC      = $(GHC)
HCFLAGS = -package ndp\
          -fglasgow-exts -O2 -funbox-strict-fields -fno-cse -fdicts-cheap\
	  -fliberate-case-threshold100 -fno-method-sharing -threaded\
          $(FLAGS)
#	  -fno-cse
# NB: method sharing and CSE can hinder fusion

# Default target
#
.PHONY: default
default: 
	echo "Error: You need to build specific tests"

# Templates
#
%.o : %.hs $(LIB)
	$(HC) -c $< $(HCFLAGS) $($(join $<,-HCFLAGS))
%.hi: %.o
	@:

# Benchmark utilities in benchutils/
#
benchutils/BenchUtils.hs-HCFLAGS =

BENCHOBJS  = benchutils/BenchUtils.o
BENCHFLAGS = -ibenchutils 

# Unit tests in unit/
#
UNIT_HCFLAGS=-ddump-simpl -no-recomp -i../../../..

unit/TestUArr.hs-HCFLAGS  = $(UNIT_HCFLAGS)
unit/TestBUArr.hs-HCFLAGS = $(UNIT_HCFLAGS)
unit/TestLoop.hs-HCFLAGS  = $(UNIT_HCFLAGS)

unit/TestBUArr: unit/TestBUArr.o
	$(HC) -o $@ $(HCFLAGS) $^
unit/TestUArr : unit/TestUArr.o
	$(HC) -o $@ $(HCFLAGS) $^
unit/TestLoop : unit/TestLoop.o
		     $(BASEOBJS)
	$(HC) -o $@ $(HCFLAGS) $^

# Simple tests in simple/
#
SIMPLE_HCFLAGS=-ddump-simpl -no-recomp

simple/MapInc.hs-HCFLAGS      =$(SIMPLE_HCFLAGS)
simple/Sum.hs-HCFLAGS         =$(SIMPLE_HCFLAGS)
simple/PrefixSum.hs-HCFLAGS   =$(SIMPLE_HCFLAGS)
simple/SegSum.hs-HCFLAGS      =$(SIMPLE_HCFLAGS)
simple/SegPrefixSum.hs-HCFLAGS=$(SIMPLE_HCFLAGS)
simple/DotProd.hs-HCFLAGS     =$(SIMPLE_HCFLAGS)

# Simple sum square fusion test in sumsq/
#
TEST_SUMSQ_HCFLAGS=-ddump-simpl -ddump-simpl-stats -no-recomp

sumsq/SumSq.hs-HCFLAGS = $(TEST_SUMSQ_HCFLAGS)

sumsq/sumsq: sumsq/SumSq.o
	$(HC) -o $@ $(HCFLAGS) $^ $(HLDFLAGS)

# Simple dot product test in dotp/
#
DOTP_HCFLAGS=$(BENCHFLAGS) -ddump-simpl -ddump-simpl-stats

dotp/DotP.hs-HCFLAGS = $(DOTP_HCFLAGS)

dotp/DotP.o: $(BENCHOBJS)

dotp/dotp: dotp/DotP.o $(BENCHOBJS)
	$(HC) -o $@ $(HCFLAGS) $^ $(HLDFLAGS)

# Parallel dot product in dotp-par
DOTP_PAR_HC_FLAGS=$(BENCHFLAGS) -idotp-par

dotp-par/DotP.hs-HCFLAGS = $(DOTP_PAR_HC_FLAGS)

dotp-par/DotP.o: dotp-par/DotPImpl.o $(BENCHOBJS)

dotp-par/dotp: dotp-par/DotP.o dotp-par/DotPImpl.o $(BENCHOBJS)
	$(HC) -o $@ $(HCFLAGS) $^ $(HLDFLAGS)

# Prime sieve in primes/
#
PRIMES_HCFLAGS=$(BENCHFLAGS) -ddump-simpl -ddump-simpl-stats

primes/Primes.hs-HCFLAGS = $(PRIMES_HCFLAGS)

primes/primes: primes/Primes.o $(BENCHOBJS)
	$(HC) -o $@ $(HCFLAGS) $^ $(HLDFLAGS)

# Sparse matrix vector multiplication in test/smvm/
#
SMVM_HCFLAGS=$(BENCHFLAGS) -ddump-simpl-stats -no-recomp
SMVM_OBJS   =smvm/SMVM_array_compr.o smvm/SMVM_array_rec.o\
	     smvm/SMVM_fusion.o smvm/SMVM_optimal.o\
	     smvm/Main.o

smvm/SMVM_array_compr.hs-HCFLAGS=$(SMVM_HCFLAGS)
smvm/SMVM_array_rec.hs-HCFLAGS  =$(SMVM_HCFLAGS)
smvm/SMVM_fusion.hs-HCFLAGS     =$(SMVM_HCFLAGS)
smvm/SMVM_optimal.hs-HCFLAGS    =$(SMVM_HCFLAGS)
smvm/Main.hs-HCFLAGS            =$(BENCHFLAGS) -ismvm

smvm/SMVM_array_compr.o: $(BENCHOBJS)
smvm/SMVM_array_rec.o  : $(BENCHOBJS)
smvm/SMVM_fusion.o     : $(BENCHOBJS)
smvm/SMVM_optimal.o    : $(BENCHOBJS)
smvm/Main.o            : $(BENCHOBJS)

smvm/Main.o: smvm/SMVM_array_compr.hi\
	     smvm/SMVM_array_rec.hi\
	     smvm/SMVM_fusion.hi\
	     smvm/SMVM_optimal.hi

smvm/smvm: $(TEST_SMVM_OBJS) $(BENCHOBJS)
	$(HC) -o $@ $(HCFLAGS) $^ $(HLDFLAGS)

# Auxilliary targets
#

.PHONY: clean
clean:
	-$(RM) unit/*.hi unit/*.o unit/TestUArr unit/TestBUArr\
	  simple/*.hi simple/*.o\
	  sumsq/*.hi sumsq/*.o sumsq/sumsq\
	  benchutils/*.hi benchutils/*.o\
	  dotp/*.hi dotp/*.o dotp/dotp\
	  dotp-par/*.hi dotp-par/*.o dotp-par/dotp\
	  primes/*.hi primes/*.o primes/primes\
	  smvm/*.hi smvm/*.o smvm/smvm
