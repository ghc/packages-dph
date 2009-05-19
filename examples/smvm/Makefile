TOPDIR = ..
BINARIES = smvm prim mksm smvm-c

HCCFLAGS = -optc-O3

smvm_DPH = smvm
smvm_SOURCES = vect.hs SMVMVect.hs

prim_DPH = prim
prim_SOURCES = prim.hs SMVMPrim.hs

smvm-c_CSOURCES = smvm-c.c

CFLAGS = -I$(TOPDIR)/../../../includes
mksm_CSOURCES = mksm.c

include $(TOPDIR)/mk/test.mk
