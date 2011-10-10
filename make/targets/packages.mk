
.PHONY : packages

dph_packages_backend = \
	dph-base \
	dph-prim-interface \
	dph-prim-seq \
	dph-prim-par
	
dph_packages_frontend = \
	dph-lifted-copy-seq \
	dph-lifted-copy-par \
	dph-lifted-vseg-seq \
	dph-lifted-vseg-par

	
dph_packages_backend_dbs = \
	$(patsubst %,%/dist/package.conf.inplace,$(dph_packages_backend))
	

# Build all the packages
.PHONY 	   : packages
packages   : $(dph_packages_backend_dbs)


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
	 ./Setup configure ; \
	 ./Setup build ; \
	 ./Setup install
	
	@echo


# -- Front end package stubs --------------------------------------------------
# Create a front-end build package
#   Both front-end source packages can be compiled against both backend packages,
#   for a total of four possible ways.
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

$(eval $(call dph_create_frontend_build,copy,seq))
$(eval $(call dph_create_frontend_build,copy,par))
$(eval $(call dph_create_frontend_build,vseg,seq))
$(eval $(call dph_create_frontend_build,vseg,par))


.PHONY	: front
front	: $(dph_packages_frontend)
