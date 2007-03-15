TESTDIR = ..
PROGS = dotp
include $(TESTDIR)/mk/test.mk

# Bench.o: Timing.hi
dotp.o: DotPSeq.hi DotPPar.hi
# Timing.hi Bench.hi

dotp: DotPSeq.o DotPPar.o $(BENCHLIB)
# Timing.o Bench.o

