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
$(DPH_DIR)/dph-$1/dph-$1.cabal : $(DPH_DIR)/dph-common/ghc.mk $(DPH_DIR)/dph-common/GNUmakefile $(DPH_DIR)/dph-common/dph-common.cabal
	$$(RM) $$(RM_OPTS_REC) $(DPH_DIR)/dph-$1 $(DPH_DIR)/dph-$1.tmp
	mkdir $(DPH_DIR)/dph-$1.tmp
	cp $(DPH_DIR)/dph-common/Setup.hs $(DPH_DIR)/dph-$1.tmp/Setup.hs
	cp $(DPH_DIR)/dph-common/LICENSE $(DPH_DIR)/dph-$1.tmp/LICENSE
	sed "s/DPHWAY/$1/g" $(DPH_DIR)/dph-common/dph-common.cabal > $(DPH_DIR)/dph-$1.tmp/dph-$1.cabal
	sed "s/common/$1/g" $(DPH_DIR)/dph-common/ghc.mk > $(DPH_DIR)/dph-$1.tmp/ghc.mk
	sed "s/common/$1/g" $(DPH_DIR)/dph-common/GNUmakefile > $(DPH_DIR)/dph-$1.tmp/GNUmakefile
	mv $(DPH_DIR)/dph-$1.tmp $(DPH_DIR)/dph-$1

$(DPH_DIR)/dph-$1/ghc.mk : $(DPH_DIR)/dph-$1/dph-$1.cabal
$(DPH_DIR)/dph-$1/GNUmakefile : $(DPH_DIR)/dph-$1/dph-$1.cabal
endif
endif

endef

$(foreach way, $(DPH_WAYS), $(eval $(call dph_create,$(way))))

.PHONY: all_$(DPH_DIR)
all_$(DPH_DIR) : $(foreach pkg, $(DPH_PACKAGES), all_$(DPH_DIR)/$(pkg))

clean : clean_$(DPH_DIR)
.PHONY: clean_$(DPH_DIR)
clean_$(DPH_DIR) : $(foreach pkg, $(DPH_BASE_PACKAGES), clean_$(DPH_DIR)/$(pkg))
	$(RM) $(RM_OPTS_REC) $(foreach way, $(DPH_WAYS), $(DPH_DIR)/dph-$(way))

distclean : clean_$(DPH_DIR)

define dph_package
ifneq "$(CLEANING)" "YES"
.PHONY: $(DPH_DIR)/$1
$(DPH_DIR)/$1 : all_$(DPH_DIR)/$1
endif
endef

$(foreach pkg, $(DPH_PACKAGES), $(eval $(call dph_package,$(pkg))))


# When compiling modules that use TH.Repr, we will try to run some TH,
# which means using the vanilla TH.Repr object files. If we are not
# building in the vanilla way then we need to be sure that the vanilla
# object files exist. These deps take care of that for us.
#
define dph_th_deps
# $1 = way  $2 = par/seq
ifneq "$1" "v"
libraries/dph/dph-$2/dist-install/build/Data/Array/Parallel/Lifted/TH/Repr.$$($1_osuf): libraries/dph/dph-$2/dist-install/build/Data/Array/Parallel/Lifted/TH/Repr.o
libraries/dph/dph-$2/dist-install/build/Data/Array/Parallel/Lifted/PArray.$${$1_osuf} : libraries/dph/dph-$2/dist-install/build/Data/Array/Parallel/Lifted/PArray.$${v_osuf}
endif

# HACKS ***********************************************************************
# The following modules use Template Haskell, or contain ANN pragmas. Both of
# these features use compile-time evaluation. During this evaluation we may
# need to load the dph-prim-* packages, but if they haven't been build yet the
# compilation will die. This results in a build race, where the compilation
# will succeed or not depending on whether another make thread has already
# completed building the dph-prim-* packages.
#
# Note that the GHC build system does NOT respect the package dependencies
# present in .cabal files. Even though the dph-common.cabal file lists
# the dph-prim-* packages as dependencies, these dependencies are silently
# ignored.
#
# The hack-around is to add the following explicit dependencies:
#   The .o for every module that uses Template Haskell or annotations must
#   must depend on the dph-prim-* GHCI libraries, so that they can be 
#   loaded at compile time.
# 
# If the dependencies are wrong you will get a build race that can result in 
# the following error:
# 
#   "inplace/bin/ghc-stage2"  ... -o .../Data/Array/Parallel/Lifted/PArray.dyn_o
#    Loading package dph-prim-seq-0.4.0 ... linking ... done.
#    Loading package dph-prim-par-0.4.0 ... <command line>: can't load .so/.DLL for: HSdph-prim-par-0.4.0-ghc6.13.20091222
#       (libHSdph-prim-seq-0.4.0-ghc6.13.20091222.so: cannot open shared object file: No such file or directory)
#
libraries/dph/dph-$2/dist-install/build/Data/Array/Parallel/Lifted/TH/Repr.$$($1_osuf): \
	$$(libraries/dph/dph-prim-$2_dist-install_GHCI_LIB)

libraries/dph/dph-$2/dist-install/build/Data/Array/Parallel/PArray/PData.$${$1_osuf} : \
	$$(libraries/dph/dph-prim-$2_dist-install_GHCI_LIB)

libraries/dph/dph-$2/dist-install/build/Data/Array/Parallel/PArray/Base.$${$1_osuf} : \
	$$(libraries/dph/dph-prim-$2_dist-install_GHCI_LIB)
endef


ifneq "$(CLEANING)" "YES"
$(foreach way, $(GhcLibWays), $(eval $(call dph_th_deps,$(way),seq)))
$(foreach way, $(GhcLibWays), $(eval $(call dph_th_deps,$(way),par)))
endif

