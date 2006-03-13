TOP=..
include $(TOP)/mk/boilerplate.mk

ALL_DIRS =
PACKAGE = ndp
VERSION = 1.0
PACKAGE_DEPS = base
SRC_HADDOCK_OPTS += -t "Haskell Hierarchical Libraries (ndp package)"

include $(TOP)/mk/target.mk
