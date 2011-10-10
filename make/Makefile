# The DPH build system.
#
#   (Build targets)
#	all		-- build the library and examples.
#	packages	-- build   the cabal packages.
#	install		-- install the cabal packages.
#
#   (Cleaning up)
#	clean		-- clean everything
#

include make/config.mk


# -- Mega Targets -------------------------------------------------------------
.PHONY : all
all : 
	@echo THREADS = $(THREADS_MAKE)
	@$(MAKE) -f make/Makefile deps
	@$(MAKE) -f make/Makefile -j $(THREADS_MAKE) \
		prim-seq prim-par \
		lifted-$(FRONTEND)


# Unlifted array primitives (backend)
.PHONY : prim-seq 
prim-seq : \
	dph-prim-seq/Data/Array/Parallel/Unlifted/Sequential.o


.PHONY : prim-par
prim-par : \
	dph-prim-par/Data/Array/Parallel/Unlifted/Parallel.o \
	dph-prim-par/Data/Array/Parallel/Unlifted/Distributed.o 
	

# Lifted array primitives (frontend)
.PHONY : lifted-copy
lifted-copy 	: dph-lifted-copy/Data/Array/Parallel.o


.PHONY : lifted-vseg 
lifted-vseg 	: dph-lifted-vseg/Data/Array/Parallel/PArray.o


# -- Source Files -------------------------------------------------------------
# Dirs that contain real source files that we want to build.
dph_src_dirs \
 = 	dph-base \
	dph-prim-seq \
	dph-prim-par \
	dph-lifted-$(FRONTEND)

# Dirs that contain .h files that we may way to #include
dph_inc_dirs	  = dph-base/include dph-prim-interface/interface

# Gather up all the .hs files.
dph_src_dirs_data = $(patsubst %,%/Data,$(dph_src_dirs))
dph_src_hs_all	  = $(shell find $(dph_src_dirs_data) -name "*.hs" -follow)
dph_src_hs	  = $(filter-out %/Unlifted.hs,$(dph_src_hs_all))


# -- Default Rules -----------------------------------------------------------
%.hi : %.o
	@true
	
%.o : %.hs
	@echo "* Compiling $<"
	@$(GHC) $(GHC_EXTS) \
		$(GHC_OPTS) \
		$(patsubst %,-I%,$(dph_inc_dirs)) \
		$(patsubst %,-i%,$(dph_src_dirs)) \
		-c $<


# -- Build Targets -----------------------------------------------------------
include make/targets/clean.mk
include make/targets/deps.mk


-include make/Makefile.deps.inc
