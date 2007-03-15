TESTDIR=..
include $(TESTDIR)/mk/common.mk

HCFLAGS = -O

.PHONY: clean all

all: libNDPBench.a

clean:
	-$(RM) Bench/*.o Bench/*.hi libNDPBench.a

libNDPBench.a: Bench/Benchmark.o Bench/Time.o Bench/Options.o
	$(RM) $@
	$(AR) q $@ $^

%.o: %.hs
	$(HC) -c $< $(HCFLAGS) $(FLAGS)

%.hi: %.o
	@:

Bench/Benchmark.o: Bench/Time.hi Bench/Options.hi


