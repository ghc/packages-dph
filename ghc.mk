DPH_DIR = libraries/dph
DPH_WAYS = par seq
DPH_BASE_PACKAGES = dph-base \
		    dph-prim-interface \
		    dph-prim-seq \
		    dph-prim-par
DPH_PACKAGES = $(DPH_BASE_PACKAGES) \
               $(foreach way, $(DPH_WAYS), dph-$(way))

define dph_create

ifneq "$(BINDIST)" "YES"
ifneq "$(CLEANING)" "YES"
$(DPH_DIR)/dph-$1/ghc.mk $(DPH_DIR)/dph-$1/GNUmakefile $(DPH_DIR)/dph-$1/dph-$1.cabal: $(DPH_DIR)/dph-common/ghc.mk $(DPH_DIR)/dph-common/GNUmakefile $(DPH_DIR)/dph-common/dph-common.cabal
	rm -rf $(DPH_DIR)/dph-$1 $(DPH_DIR)/dph-$1.tmp
	mkdir $(DPH_DIR)/dph-$1.tmp
	cp $(DPH_DIR)/dph-common/Setup.hs $(DPH_DIR)/dph-$1.tmp/Setup.hs
	cp $(DPH_DIR)/dph-common/LICENSE $(DPH_DIR)/dph-$1.tmp/LICENSE
	sed "s/DPHWAY/$1/g" $(DPH_DIR)/dph-common/dph-common.cabal > $(DPH_DIR)/dph-$1.tmp/dph-$1.cabal
	sed "s/common/$1/g" $(DPH_DIR)/dph-common/ghc.mk > $(DPH_DIR)/dph-$1.tmp/ghc.mk
	sed "s/common/$1/g" $(DPH_DIR)/dph-common/GNUmakefile > $(DPH_DIR)/dph-$1.tmp/GNUmakefile
	mv $(DPH_DIR)/dph-$1.tmp $(DPH_DIR)/dph-$1
endif
endif

endef

$(foreach way, $(DPH_WAYS), $(eval $(call dph_create,$(way))))

.PHONY: all_$(DPH_DIR)
all_$(DPH_DIR) : $(foreach pkg, $(DPH_PACKAGES), all_$(DPH_DIR)/$(pkg))

clean : clean_$(DPH_DIR)
.PHONY: clean_$(DPH_DIR)
clean_$(DPH_DIR) : $(foreach pkg, $(DPH_BASE_PACKAGES), clean_$(DPH_DIR)/$(pkg))
	$(RM) -rf $(foreach way, $(DPH_WAYS), $(DPH_DIR)/dph-$(way))

distclean : clean_$(DPH_DIR)

define dph_package
ifneq "$(CLEANING)" "YES"
.PHONY: $(DPH_DIR)/$1
$(DPH_DIR)/$1 : all_$(DPH_DIR)/$1
endif
endef

$(foreach pkg, $(DPH_PACKAGES), $(eval $(call dph_package,$(pkg))))



