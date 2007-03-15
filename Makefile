TOP=..
include $(TOP)/mk/boilerplate.mk
include ndp.mk

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

PACKAGE_DEPS = base

include $(TOP)/mk/target.mk

SRC_HC_OPTS += $(NDPFLAGS)

SRC_HADDOCK_OPTS += -t "Haskell Hierarchical Libraries (ndp package)"

