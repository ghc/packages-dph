TOPDIR = ..
include $(TOPDIR)/mk/test.mk

.PHONY: default
default: SumSq_seq SumSq_par

SumSq_seq: SumSq.hs
	$(HC) --make -o $@ $< $(DPH_FLAGS) -package dph-prim-seq -ddump-simpl

SumSq_par: SumSq.hs
	$(HC) --make -o $@ $< $(DPH_FLAGS) -package dph-prim-par -ddump-simpl