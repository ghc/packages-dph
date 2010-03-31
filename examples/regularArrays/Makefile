TOPDIR = ..
BINARIES = raBench raTest saBench laplace

# -----------------
raBench_DPH 	= raBench
raBench_SOURCES =  ReglibBench.hs Array.hs ArrayExamples.hs DArray.hs DArrayExamples.hs 

raBench_seq_FLAGS = -fsimplifier-phases=3 -package dph-base
raBench_par_FLAGS = -fsimplifier-phases=3 -package dph-base


# -----------------
raTest_DPH 	= raTest
raTest_SOURCES	=  ReglibTest.hs Array.hs ArrayExamples.hs DArray.hs DArrayExamples.hs 

raTest_seq_FLAGS = -fsimplifier-phases=3 -package dph-base
raTest_par_FLAGS = -fsimplifier-phases=3 -package dph-base



# -----------------
saBench_DPH 	= saBench
saBench_SOURCES =  ArrayBench.hs

saBench_seq_FLAGS = -fsimplifier-phases=3 -package dph-base
saBench_par_FLAGS = -fsimplifier-phases=3 -package dph-base



# ----------------
laplace_DPH	= laplace
laplace_SOURCES	= \
	examples/laplace/Main.hs \
	examples/laplace/SolveArray.hs \
	examples/laplace/SolveDArray.hs \
	examples/laplace/SolveCArray.hs \
	examples/laplace/SolveCArrayFlatDim.hs \
	examples/lib/PPM.hs \

laplace_seq_FLAGS = -iexamples/laplace -iexamples/lib -fsimplifier-phases=3 -package dph-base
laplace_par_FLAGS = -iexamples/laplace -iexamples/lib -fsimplifier-phases=3 -package dph-base
	

include $(TOPDIR)/mk/test.mk
