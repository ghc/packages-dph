TESTDIR = ..
PROGS = Primes
include $(TESTDIR)/mk/test.mk

PrimesVect.o: PrimesVect.hs
	../../../../compiler/ghc-inplace -c PrimesVect.hs -fglasgow-exts -fbang-patterns -O2 -funbox-strict-fields -fdicts-cheap -fno-method-sharing  -fmax-simplifier-iterations20 -threaded -XTypeFamilies -fcpr-off  -package ndp -package ghc-prim -no-recomp -i../lib 

Primes: Primes.o  PrimesVect.o
	../../../../compiler/ghc-inplace -o Primes -fglasgow-exts -fbang-patterns -O2 -funbox-strict-fields -fdicts-cheap -fno-method-sharing -fno-spec-constr-threshold -fmax-simplifier-iterations20 -threaded -XTypeFamilies -fcpr-off  -package ndp -no-recomp -i../lib Primes.o PrimesVect.o -L../lib -lNDPBench  -package ndp  -package ghc-prim

