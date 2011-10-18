
.PHONY : packages

dph_packages_backend = \
	dph-base \
	dph-prim-interface \
	dph-prim-seq \
	dph-prim-par
	
dph_packages_frontend = \
	dph-seq \
	dph-par \
	dph-lifted-vseg-seq \
	dph-lifted-vseg-par

dph_packages = \
	$(dph_packages_backend) \
	$(dph_packages_frontend)
	
dph_packages_dbs = \
	$(patsubst %,%/dist/package.conf.inplace,$(dph_packages))
	

# Build all the packages
.PHONY 	   : packages
packages   : $(dph_packages_dbs)


# Unregister all the DPH packages
.PHONY	   : unregsiter
unregister :
	@for p in $(dph_packages_backend); do \
		$(GHC_PKG) unregister $$p --force; \
	done


# Build the backend packages	
%/dist/package.conf.inplace : %
	@cd $(patsubst %/dist/package.conf.inplace,%,$@) ; \
	 $(GHC_DPH) --make Setup.hs ; \
	 ./Setup configure --user ; \
	 ./Setup build ; \
	 ./Setup install
	
	@echo


# -- Front end package stubs --------------------------------------------------
# Create a front-end build package stub
#   Both front-end source packages can be compiled against both backend packages,
#   for a total of four possible ways. We build these by copying the common
#   cabal file out to a new directory, and setting the source dir field in 
#   that cabal file to point to the location of the original sources.
define dph_create_frontend_build
dph-lifted-$1-$2 :
	@echo "* Creating frontend package $$@"
	@mkdir -p dph-lifted-$1-$2.tmp
	@cp dph-lifted-$1/Setup.hs dph-lifted-$1-$2.tmp/Setup.hs
	@cp dph-lifted-$1/LICENSE  dph-lifted-$1-$2.tmp/LICENSE
	
	@sed "s/DPHWAY/$2/g" dph-lifted-$1/dph-lifted-$1.cabal \
		> dph-lifted-$1-$2.tmp/dph-lifted-$1-$2.cabal

	@mv dph-lifted-$1-$2.tmp dph-lifted-$1-$2
endef

$(eval $(call dph_create_frontend_build,vseg,seq))
$(eval $(call dph_create_frontend_build,vseg,par))


define dph_create_frontend_common
dph-$1 : dph-common/Setup.hs dph-common/LICENSE dph-common/dph-common.cabal
	@echo "* Creating frontend package $$@"
	@rm    -Rf dph-$1 dph-$1.tmp
	@mkdir -p  dph-$1.tmp
	@cp dph-common/Setup.hs dph-$1.tmp/Setup.hs
	@cp dph-common/LICENSE  dph-$1.tmp/LICENSE
	
	@sed "s/DPHWAY/$1/g" dph-common/dph-common.cabal \
		> dph-$1.tmp/dph-$1.cabal

	@mv dph-$1.tmp dph-$1
endef

$(eval $(call dph_create_frontend_common,seq))
$(eval $(call dph_create_frontend_common,par))


.PHONY	: front
front	: $(dph_packages_frontend)
