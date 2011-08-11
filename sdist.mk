# The following works in a GHC build tree.  To use the installed GHC
# instead, replace with GHC=ghc.
GHC=ghc

# The "-pkg" suffix is to avoid clashing with the dph-seq and dph-par
# directories created by the GHC build system.

all : dph-seq dph-par sdist

define dph_create

dph-$1 :
	rm -rf dph-$1 dph-$1.tmp
	cp -r dph-common dph-$1.tmp
	rm dph-$1.tmp/dph-common.cabal
	sed -e "s/DPHWAY/$1/g;/HS-Source-Dirs/d" dph-common/dph-common.cabal > dph-$1.tmp/dph-$1.cabal
	mv dph-$1.tmp dph-$1

endef

$(eval $(call dph_create,seq))
$(eval $(call dph_create,par))

clean :
	rm -rf dph-seq dph-par

PACKAGES = dph-base dph-prim-interface dph-prim-seq dph-prim-par dph-seq dph-par dph

sdist : dph-seq dph-par
	for i in $(PACKAGES); do \
	    ( cd $$i; \
	      cabal configure --with-ghc=$(GHC); \
	      cabal sdist; ) \
        done
	@echo
	@echo "packages are:" $(foreach i, $(PACKAGES), $i/dist/*.tar.gz)
