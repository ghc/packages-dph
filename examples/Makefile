TESTDIR=.
include $(TESTDIR)/mk/test.mk

SUBDIRS = concomp dotp primes smvm qsort barneshut

.PHONY: all bench clean

all: bench
	for i in $(SUBDIRS) ; do \
	  $(MAKE) -C $$i    ;    \
	done

bench:
	$(MAKE) -C lib

clean:
	for i in $(SUBDIRS) ; do \
	  $(MAKE) -C $$i clean ; \
	done
	$(MAKE) -C lib clean


