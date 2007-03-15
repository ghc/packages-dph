include $(TESTDIR)/mk/common.mk
HCFLAGS = $(NDPFLAGS) $(TESTFLAGS) -package ndp -no-recomp -i$(BENCHDIR)
HLDFLAGS += -L$(BENCHDIR) -lNDPBench

.PHONY: clean all bench

all: bench $(PROGS)

clean:
	-$(RM) *.hi *.o $(PROGS)

%.o: %.hs $(NDPLIB) $(BENCHLIB)
	$(HC) -c $< $(HCFLAGS) $(FLAGS)

%.o: %.c
	$(HC) -c $< $(HCCFLAGS) $(FLAGS)

%: %.c
	$(HC) -o $@ $(HCCFLAGS) $^ $(HLDFLAGS)

%: %.o
	$(HC) -o $@ $(HCFLAGS) $^ $(HLDFLAGS)

%.hi: %.o
	@:

bench:
	cd $(BENCHDIR) && $(MAKE)

