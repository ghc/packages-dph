TESTDIR = ..
PROGS = mksm smvm-c smvm
HCCFLAGS = -optc-O3
include $(TESTDIR)/mk/test.mk

# Bench.o: Timing.hi
smvm.o: SMVMPar.hi SMVMSeq.hi 
# Timing.hi Bench.hi

smvm: smvm.o SMVMPar.o SMVMSeq.o
# Timing.o Bench.o

