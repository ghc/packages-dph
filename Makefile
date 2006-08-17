TOP=..
include $(TOP)/mk/boilerplate.mk

ALL_DIRS = \
	Data/Array \
	Data/Array/Parallel \
	Data/Array/Parallel/Base \
	Data/Array/Parallel/Stream \
	Data/Array/Parallel/Stream/Flat \
	Data/Array/Parallel/Arr \
	Data/Array/Parallel/Unlifted \
	Data/Array/Parallel/Unlifted/Flat \
	Data/Array/Parallel/Unlifted/Segmented \
	Data/Array/Parallel/Unlifted/Distributed \
	Data/Array/Parallel/Unlifted/Parallel

PACKAGE = ndp
VERSION = 1.0

SRC_HC_OPTS += -fglasgow-exts -O2 -funbox-strict-fields\
	       -fliberate-case-threshold100 -fdicts-cheap -fno-method-sharing\
               -threaded

PACKAGE_DEPS = base
SRC_HADDOCK_OPTS += -t "Haskell Hierarchical Libraries (ndp package)"

include $(TOP)/mk/target.mk
