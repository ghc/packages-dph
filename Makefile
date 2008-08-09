
create: dph_par dph_seq

.PHONY: dph_par dph_seq

dph_par dph_seq:
	rm -rf $@
	rm -rf $@.tmp
	mkdir $@.tmp
	cp dph/Setup.hs $@.tmp/
	cpp -DUSE_$@ < dph/dph.cabal | grep -v "^#" > $@.tmp/dph.cabal
	mv $@.tmp $@

clean:
	rm -rf dph_par dph_seq

