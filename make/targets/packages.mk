
.PHONY : packages

dph_packages = \
	dph-base \
	dph-prim-interface \
	dph-prim-seq \
	dph-prim-par
	
dph_package_dbs	= \
	$(patsubst %,%/dist/package.conf.inplace,$(dph_packages))


# Build all the packages
.PHONY 	   : packages
packages   : $(dph_package_dbs)


# Unregister all the DPH packages
.PHONY	   : unregsiter
unregister :
	@for p in $(dph_packages); do \
		$(GHC_PKG) unregister $$p --force; \
	done

# Build the primitive packages	
%/dist/package.conf.inplace : %
	@cd $(patsubst %/dist/package.conf.inplace,%,$@) ; \
	 $(GHC_DPH) --make Setup.hs ; \
	 ./Setup configure ; \
	 ./Setup build ; \
	 ./Setup install
	
	@echo


# -- Source Distributions -----------------------------------------------------
# Make front-end packages that can be distributed as source tarballs.
#   Both frontend packages can be compiled against both backend packages, 
#   for four possible combinations:
#    $1 <- {copy, vseg}
#    $2 <- {seq,  par}
#   When we build these packages in the local tree, the source dirs are
#   in the cabal files are set so that 

define dph_create_frontend_sdist
sdist/dph-lifted-$1-$2 :
	mkdir -p sdist
	rm -rf sdist/dph-lifted-$1-$2 sdist/dph-lifted-$1-$2.tmp
	cp -r  dph-lifted-$1          sdist/dph-lifted-$1-$2.tmp
	rm     sdist/dph-lifted-$1-$2.tmp/dph-lifted-$1.cabal
	sed -e "s/DPHWAY/$2/g;/HS-Source-Dirs/d" dph-lifted-$1/dph-lifted-$1.cabal \
		> sdist/dph-lifted-$1-$2.tmp/dph-lifted-$1-$2.cabal
	mv sdist/dph-lifted-$1-$2.tmp sdist/dph-lifted-$1-$2
endef

$(eval $(call dph_create_frontend_sdist,copy,seq))
$(eval $(call dph_create_frontend_sdist,copy,par))
$(eval $(call dph_create_frontend_sdist,vseg,seq))
$(eval $(call dph_create_frontend_sdist,vseg,par))

.PHONY : sdist
sdist : sdist/dph-lifted-copy-seq \
	sdist/dph-lifted-copy-par \
	sdist/dph-lifted-vseg-seq \
	sdist/dph-lifted-vseg-par

