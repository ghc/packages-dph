# Default build configuration.
#   This file is under version control.
#   If you want to override these options then create a file make/config-override.mk
#   and assign the appropdiate variables there.

# -- Tools --------------------------------------------------------------------
GHC		= ghc

# -- Backend ------------------------------------------------------------------
# What unlifted backend to use when compiling the dph-common libraries.
# Options are {par, seq}
BACKEND		= par

# What lifted frontend to use for vectorised code.
# Options are {copy, vseg}
FRONTEND	= vseg

# -- Flags --------------------------------------------------------------------
# How many threads to use with make
THREADS_MAKE	= 4

# Optimisations to compile with.
GHC_OPTS = \
	-Odph \
	-fno-liberate-case

# GHC language extensions that DPH code needs.
GHC_EXTS	= \
	-XCPP \
	-XBangPatterns \
	-XNoMonomorphismRestriction \
	-XRankNTypes \
	-XTypeFamilies \
	-XFlexibleInstances \
	-XFlexibleContexts \
	-XMagicHash \
	-XUnboxedTuples 


# -- Override -----------------------------------------------------------------
# Override the above config.
-include make/config-override.mk
