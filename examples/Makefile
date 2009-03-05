SUBDIRS = barnesHut concomp dotp primes primespj qsort quickhull smvm sumsq

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


