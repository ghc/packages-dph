TOPDIR = ..
BINARIES = sumsq prim sumsq-c

sumsq_DPH = sumsq
sumsq_SOURCES = vect.hs SumSqVect.hs
sumsq_seq_FLAGS = -fsimplifier-phases=3
sumsq_par_FLAGS = -fsimplifier-phases=3

prim_DPH = prim
prim_SOURCES = prim.hs SumSqPrim.hs

CFLAGS = -I$(TOPDIR)/../../../includes
sumsq-c_CSOURCES = sumsq-c.c

include $(TOPDIR)/mk/test.mk
