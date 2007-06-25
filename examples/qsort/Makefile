TESTDIR = ..
PROGS = QSort
HCCFLAGS = -optc-O3 -fglasgow-exts -fbang-patterns -O2 -funbox-strict-fields -fliberate-case-threshold100 -fdicts-cheap -fno-method-sharing  -fmax-simplifier-iterations6 -threaded -haddock
include $(TESTDIR)/mk/test.mk

QSortSeq.o: 

QSortMain.o: QSortSeq.hi

QSort: QSortMain.hs QSortSeq.hs
	~/ghc/compiler/ghc-inplace $(HCCFLAGS) --make -package ndp -fglasgow-exts -o QSort QSortMain.hs 
#-ddump-simpl 