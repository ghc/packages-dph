TESTDIR = ..
PROGS = QSort
HCCFLAGS = -optc-O3
include $(TESTDIR)/mk/test.mk

QSort.o: QSortPar.hi QSortSeq.hi QSortVect.hi

QSort: QSort.o QSortPar.o QSortSeq.o QSortVect.o
