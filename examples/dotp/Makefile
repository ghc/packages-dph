TESTDIR = ..
PROGS = dotp
include $(TESTDIR)/mk/test.mk

dotp.o: DotPSeq.hi DotPPar.hi

dotp: DotPSeq.o DotPPar.o $(BENCHLIB)

