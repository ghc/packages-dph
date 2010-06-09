TOPDIR = ..
BINARIES = quickhull quickhullSVG QuickHull gen

HCCFLAGS = -optc-O3

quickhull_DPH = quickhull
quickhull_SOURCES = vect.hs QuickHullVect.hs Types.hs

QuickHull_SOURCES = QuickHull.hs
QuickHull_FLAGS   = -O2 -package dph-prim-seq

quickhullSVG_DPH = quickhullSVG
quickhullSVG_SOURCES = vectSVG.hs QuickhullVect.hs Types.hs SVG.hs TestData.hs

gen_SOURCES = gen.hs TestData.hs
gen_FLAGS = -fdph-seq


include $(TOPDIR)/mk/test.mk


# -- C version
quickhullc_SOURCES = legacy/c/Main.c legacy/c/QuickHull.c legacy/c/SVG.c legacy/c/Timing.c
quickhullc_HEADER  = legacy/c/Timing.h legacy/c/Vector.h

seq/quickhullc : $(quickhullc_SOURCES) $(quickhullc_HEADER)
	gcc -std=c99 -O3 $(quickhullc_SOURCES) -o seq/quickhullc -lm


# -- Vector versions
quickhullv_SOURCES = \
	legacy/vector/Main.hs \
	legacy/vector/QuickHullIO.hs \
	legacy/vector/QuickHullSplit.hs \
	legacy/vector/QuickHullVector.hs \
	legacy/vector/TestDataVector.hs \
	legacy/vector/SVG.hs \
	legacy/vector/Timing.hs
	
seq/quickhullv : $(quickhullv_SOURCES)
	ghc -Odph -ilegacy/vector --make legacy/vector/Main.hs -o seq/quickhullv \
		-package dph-prim-seq -package dph-seq -threaded

