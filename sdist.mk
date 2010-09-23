# The following works in a GHC build tree.  To use the installed GHC
# instead, replace with GHC=ghc.
GHC=../../../inplace/bin/ghc-stage2

# The "-pkg" suffix is to avoid clashing with the dph-seq and dph-par
# directories created by the GHC build system.

all : dph-seq-pkg dph-par-pkg sdist

define dph_create

dph-$1-pkg :
	rm -rf dph-$1-pkg dph-$1-pkg.tmp
	cp -r dph-common dph-$1-pkg.tmp
	rm dph-$1-pkg.tmp/dph-common.cabal
	sed -e "s/DPHWAY/$1/g;/HS-Source-Dirs/d" dph-common/dph-common.cabal > dph-$1-pkg.tmp/dph-$1-pkg.cabal
	sed "s/common/$1/g" dph-common/ghc.mk > dph-$1-pkg.tmp/ghc.mk
	sed "s/common/$1/g" dph-common/GNUmakefile > dph-$1-pkg.tmp/GNUmakefile
	mv dph-$1-pkg.tmp dph-$1-pkg

endef

$(eval $(call dph_create,seq))
$(eval $(call dph_create,par))

clean :
	rm -rf dph-seq-pkg dph-par-pkg

PACKAGES = dph-base dph-prim-interface dph-prim-seq dph-prim-par dph-seq-pkg dph-par-pkg dph

sdist : dph-seq-pkg dph-par-pkg
	for i in $(PACKAGES); do \
	    ( cd $$i; \
	      cabal configure --with-ghc=$(GHC); \
	      cabal sdist; ) \
        done
	@echo
	@echo "packages are:" $(foreach i, $(PACKAGES), $i/dist/*.tar.gz)
