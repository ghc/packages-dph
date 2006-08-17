GHC = ../../../../../../../../compiler/stage1/ghc-inplace
LIB = ../../../../../../libHSndp.a

HC      = $(GHC)
HCFLAGS = -package ndp\
          -fglasgow-exts -O2 -funbox-strict-fields\
	  -fliberate-case-threshold100 -fno-method-sharing -fdicts-cheap\
          -threaded

ALGS = AwShU.hs AwShUP.hs HybU.hs HybUP.hs

.PHONY: all clean

all: mkg concomp

%.o: %.hs $(LIB)
	$(HC) -c $< $(HCFLAGS) -no-recomp
%.hi: %.o
	@:

Main.o: Graph.hi Timing.hi Bench.hi $(ALGS:.hs=.hi)

concomp: Main.o Graph.o Timing.o Bench.o $(ALGS:.hs=.o)
	$(HC) -o $@ $^ $(HCFLAGS)

Bench.o: Timing.hi
MkG.o: Graph.hi

mkg: MkG.o Graph.o
	$(HC) -o $@ $^ $(HCFLAGS)

clean:
	rm -f *.o *.hi concomp mkg
