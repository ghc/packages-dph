TOPDIR = ..
BINARIES = quickhull quickhullSVG QuickHull gen

HCCFLAGS = -optc-O3

quickhull_DPH = quickhull
quickhull_SOURCES = vect.hs QuickHullVect.hs Types.hs

QuickHull_SOURCES = QuickHull.hs
QuickHull_FLAGS   = -O2

quickhullSVG_DPH = quickhullSVG
quickhullSVG_SOURCES = vectSVG.hs QuickhullVect.hs Types.hs SVG.hs TestData.hs

gen_SOURCES = gen.hs TestData.hs
gen_FLAGS = -fdph-seq

include $(TOPDIR)/mk/test.mk
