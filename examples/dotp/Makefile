TESTDIR = ..
PROGS = dotp
include $(TESTDIR)/mk/test.mk

dotp.o: DotPSeq.hi DotPPar.hi DotPVect.hi

dotp: DotPSeq.o DotPPar.o DotPVect.o $(BENCHLIB)

