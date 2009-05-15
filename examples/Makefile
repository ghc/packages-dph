# SUBDIRS = barnesHut concomp dotp primes primespj qsort quickhull smvm sumsq
# we currently don't have all benchmarks working
SUBDIRS = sumsq dotp smvm

.PHONY: all bench clean

all: bench
	@for i in $(SUBDIRS) ; do \
	  echo $(MAKE) -C $$i;    \
	  $(MAKE) -C $$i;         \
	done

bench:
	$(MAKE) -C lib

clean:
	@for i in $(SUBDIRS) ; do     \
	  echo $(MAKE) -C $$i clean;  \
	  $(MAKE) -C $$i clean;       \
	done
	$(MAKE) -C lib clean


