
# Build dependencies for all source files
.PHONY 	: deps
deps	: make/Makefile.deps

PRIM_Unlifted	= dph-prim-$(BACKEND)/Data/Array/Parallel/Unlifted.hs 

make/Makefile.deps : $(dph_src_hs) $(PRIM_Unlifted)
	@echo "* Building dependencies"

	$(GHC) $(GHC_EXTS) \
		$(patsubst %,-I%,$(dph_inc_dirs)) \
		$(patsubst %,-i%,$(dph_src_dirs)) \
		-M $^ -dep-makefile -optdepmake/Makefile.deps

	@rm -f make/Makefile.deps.bak
	@cp make/Makefile.deps make/Makefile.deps.inc
	@echo
