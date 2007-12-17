TESTDIR = ..
PROGS = quickhull
HCCFLAGS = -optc-O3
include $(TESTDIR)/mk/test.mk

quickhull.o: Types.hi QH.hi
QH.o: Types.hi

quickhull: quickhull.o QH.o Types.o

