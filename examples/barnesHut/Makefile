TESTDIR = ..
PROGS = BarnesHut
HCCFLAGS = -optc-O3
include $(TESTDIR)/mk/test.mk

BarnesHut.o: BarnesHutGen.hi BarnesHutPar.hi BarnesHutGen.hi BarnesHutSeq.hi BarnesHutVect.hi BarnesHutList.hi

BarnesHut: BarnesHut.o BarnesHutGen.o BarnesHutSeq.o BarnesHutVect.o BarnesHutList.o BarnesHutPar.o

