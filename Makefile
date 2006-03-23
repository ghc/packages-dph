TOP=..
include $(TOP)/mk/boilerplate.mk

ALL_DIRS = \
	Data/Array \
	Data/Array/Parallel \
	Data/Array/Parallel/Base \
	Data/Array/Parallel/Distributed \
	Data/Array/Parallel/Monadic \
	Data/Array/Parallel/Declarative \
	Data/Array/Parallel/Unlifted

PACKAGE = ndp
VERSION = 1.0

SRC_HC_OPTS += -fglasgow-exts -O2 -funbox-strict-fields\
	       -fliberate-case-threshold100 -fno-method-sharing

PACKAGE_DEPS = base
SRC_HADDOCK_OPTS += -t "Haskell Hierarchical Libraries (ndp package)"

include $(TOP)/mk/target.mk
