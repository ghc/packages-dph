GHC = ../../../../../../../compiler/stage2/ghc-inplace
LIB = ../../../../../libHSndp.a
INC = ../../../../..

HC      = $(GHC)
HCFLAGS = -fglasgow-exts -package QuickCheck -package template-haskell -i../../../../.. -v0

TESTSUITE = Testsuite/Utils.hs \
            Testsuite/Testcase.hs \
            Testsuite/Preproc.hs \
            Testsuite.hs

TESTSUITE_OBJS = $(TESTSUITE:.hs=.o)

TESTS = $(wildcard tests/*.hs)
TEST_OBJS = $(TESTS:.hs=.o)
LOGS = $(patsubst %.hs,%.log,$(notdir $(TESTS)))

.PHONY: default testsuite

default: $(LOGS)
	@:

testsuite: $(TESTSUITE_OBJS)

Testsuite.o: $(filter-out Testsuite.o,$(TESTSUITE_OBJS))

%.o : %.hs $(LIB)
	$(HC) -c $< $(HCFLAGS) $($(join $<,-HCFLAGS))
%.hi: %.o
	@:

$(TEST_OBJS) : testsuite

tst-%: tests/%.o
	$(HC) -o $@ $(HCFLAGS) $< $(TESTSUITE_OBJS) $(LIB)
	rm -f $<

%.log: tst-%
	@rm -f $@
	@echo "======== Testing  $(basename $@) ========"
	@./$< | tee $@ | { grep -v '\.\.\. pass' || true; }
	@echo "======== Finished $(basename $@) ========"
	@rm -f $<

