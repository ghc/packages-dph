NDPDIR = $(TESTDIR)/../../../..
BENCHDIR = $(TESTDIR)/lib

NDPLIB = $(NDPDIR)/libHSndp.a
BENCHLIB = $(BENCHDIR)/libNDPBench.a
HC = $(NDPDIR)/../../compiler/ghc-inplace

include $(NDPDIR)/ndp.mk

