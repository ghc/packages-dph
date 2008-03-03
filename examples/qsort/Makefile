TESTDIR = ..
PROGS = QSort
include $(TESTDIR)/mk/test.mk

QSortPar.o: QSortPar.hs
	../../../../compiler/ghc-inplace -c QSortPar.hs -fglasgow-exts -fbang-patterns -O2 -funbox-strict-fields -fdicts-cheap -fno-method-sharing  -fmax-simplifier-iterations20 -threaded -XTypeFamilies -fcpr-off  -package ndp -no-recomp -i../lib 

QSortSeq.o: QSortSeq.hs
	../../../../compiler/ghc-inplace -c QSortSeq.hs -fglasgow-exts -fbang-patterns -O2 -funbox-strict-fields -fdicts-cheap -fno-method-sharing  -fmax-simplifier-iterations20 -threaded -XTypeFamilies -fcpr-off  -package ndp -no-recomp -i../lib 

QSort.o: QSortSeq.hi

QSort: QSort.o QSortPar.o QSortSeq.o
	../../../../compiler/ghc-inplace -o QSort -fglasgow-exts -fbang-patterns -O2 -funbox-strict-fields -fdicts-cheap -fno-method-sharing -fno-spec-constr-threshold -fmax-simplifier-iterations20 -threaded -XTypeFamilies -fcpr-off  -package ndp -no-recomp -i../lib QSort.o QSortPar.o QSortSeq.o -L../lib -lNDPBench  -package ndp


