NDPDIR = $(TESTDIR)/..
NDPVERSION = 0.1
BENCHDIR = $(TESTDIR)/lib

NDPLIB = $(NDPDIR)/dist/build/libHSndp-$(NDPVERSION).a
BENCHLIB = $(BENCHDIR)/libNDPBench.a
HC = $(NDPDIR)/../../compiler/ghc-inplace

include $(NDPDIR)/ndp.mk

