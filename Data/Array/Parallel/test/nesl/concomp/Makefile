GHC = ../../../../../../../../compiler/stage1/ghc-inplace
LIB = ../../../../../../libHSndp.a

HC      = $(GHC)
HCFLAGS = -package ndp\
          -fglasgow-exts -O2 -funbox-strict-fields\
	  -fliberate-case-threshold100 -fno-method-sharing -threaded

ALGS = AwShU.hs AwShUP.hs HybU.hs HybUP.hs

.PHONY: all clean

all: mkg concomp

%.o: %.hs $(LIB)
	$(HC) -c $< $(HCFLAGS) -no-recomp
%.hi: %.o
	@:

Main.o: Graph.hi Timing.hi $(ALGS:.hs=.hi)

concomp: Main.o Graph.o Timing.o $(ALGS:.hs=.o)
	$(HC) -o $@ $^ $(HCFLAGS)

MkG.o: Graph.hi

mkg: MkG.o Graph.o
	$(HC) -o $@ $^ $(HCFLAGS)

clean:
	rm -f *.o *.hi concomp mkg
