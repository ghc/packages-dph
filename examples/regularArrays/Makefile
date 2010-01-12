TOPDIR = ..
BINARIES = raBench raTest saBench


raBench_seq_FLAGS = -fsimplifier-phases=3 -package dph-base
raBench_par_FLAGS = -fsimplifier-phases=3 -package dph-base


raBench_DPH = raBench
raBench_SOURCES =  ReglibBench.hs Array.hs ArrayExamples.hs DArray.hs DArrayExamples.hs Hierarchical.hs

raTest_seq_FLAGS = -fsimplifier-phases=3 -package dph-base
raTest_par_FLAGS = -fsimplifier-phases=3 -package dph-base



raTest_DPH = raTest
raTest_SOURCES =  ReglibTest.hs Array.hs ArrayExamples.hs DArray.hs DArrayExamples.hs Hierarchical.hs

saBench_seq_FLAGS = -fsimplifier-phases=3 -package dph-base
saBench_par_FLAGS = -fsimplifier-phases=3 -package dph-base


saBench_DPH = saBench
saBench_SOURCES =  ArrayBench.hs

include $(TOPDIR)/mk/test.mk
