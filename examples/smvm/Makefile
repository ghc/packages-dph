TESTDIR = ..
PROGS = mksm smvm-c smvm
HCCFLAGS = -optc-O3
include $(TESTDIR)/mk/test.mk

smvm.o: SMVMPar.hi SMVMSeq.hi 

smvm: smvm.o SMVMPar.o SMVMSeq.o

