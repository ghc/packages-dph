GHC = ../../../../../../../compiler/stage2/ghc-inplace
NDPDIR = ../../../../..
NDPLIB = $(NDPDIR)/libHSndp.a

HC      = $(GHC)
HCFLAGS = -fglasgow-exts -package QuickCheck -package template-haskell \
          -i$(NDPDIR) -v0
OPTFLAGS = -O2 -funbox-strict-fields \
           -fliberate-case-threshold100 -fno-method-sharing


TESTSUITE = Testsuite/Utils.hs \
            Testsuite/Testcase.hs \
            Testsuite/Preproc.hs \
            Testsuite.hs

TESTSUITE_OBJS = $(TESTSUITE:.hs=.o)

TESTS = $(wildcard tests/*.hs)
TEST_MODS = $(notdir $(TESTS))
OPT = $(TEST_MODS:.hs=-opt)
UNOPT = $(TEST_MODS:.hs=-unopt)
# we want the tests to be run in the right order
ALL = $(TEST_MODS:.hs=-all)

TESTMAIN = 'System.Environment.withArgs (words "$(run)") main'

.PHONY: default unopt opt all testsuite

default: unopt

all: $(ALL)

unopt: $(UNOPT)

opt: $(OPT)

testsuite: $(TESTSUITE_OBJS)

Testsuite.o: $(filter-out Testsuite.o,$(TESTSUITE_OBJS))

%.o : %.hs $(NDPLIB)
	$(HC) -c $< $(HCFLAGS)

%-opt.o: %.hs $(NDPLIB) testsuite
	$(HC) -o $@ -c $< $(HCFLAGS) $(OPTFLAGS)

%.hi: %.o
	@:

$(TEST_OBJS) : testsuite

%-all: %-unopt %-opt
	@:

%-unopt:
	@echo "======== Testing  $(patsubst %-unopt,%,$@) (interpreted) ========"
	@$(HC) -e $(TESTMAIN) $(patsubst %-unopt,tests/%.hs,$@) $(HCFLAGS) \
		| tee $@.log | { grep -v '\.\.\. pass' || true; }
	@echo "======== Finished $(patsubst %-unopt,%,$@) (interpreted) ========"

%-opt: tests/%-opt.o
	@$(HC) -o tst $(HCFLAGS) $< $(TESTSUITE_OBJS) $(NDPLIB)
	@echo "======== Testing  $(patsubst %-opt,%,$@) (optimised) ========"
	@./tst | tee $@ | { grep -v '\.\.\. pass' || true; }
	@echo "======== Finished $(patsubst %-opt,%,$@) (optimised) ========"
	@rm -f tst $<

