TOP=../..
include $(TOP)/mk/config.mk

create: dph-par dph-seq

.PHONY: dph-par dph-seq

dph-par dph-seq:
	rm -rf $@
	rm -rf $@.tmp
	mkdir $@.tmp
	cp dph/Setup.hs $@.tmp/
	$(CPP) $(RAWCPP_FLAGS) -P -DUSE_$(subst -,_,$@) -x c dph/dph.cabal > $@.tmp/dph.cabal
	mv $@.tmp $@

clean:
	rm -rf dph-par dph-seq

