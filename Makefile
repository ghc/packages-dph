
create: dph-par dph-seq

.PHONY: dph-par dph-seq

dph-par dph-seq:
	rm -rf $@
	rm -rf $@.tmp
	mkdir $@.tmp
	cp dph/Setup.hs $@.tmp/
	cpp -DUSE_$(subst -,_,$@) < dph/dph.cabal | grep -v "^#" > $@.tmp/dph.cabal
	mv $@.tmp $@

clean:
	rm -rf dph-par dph-seq

