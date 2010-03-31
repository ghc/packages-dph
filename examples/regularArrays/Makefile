TOPDIR = ..
BINARIES = raBench raTest saBench laplace mmult fft

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
	

# -----------------
mmult_DPH	= mmult
mmult_SOURCES	= \
	examples/mmult/Main.hs
	
mmult_seq_FLAGS = -iexamples/mmult -iexamples/lib -fsimplifier-phases=3 -package dph-base
mmult_par_FLAGS = -iexamples/mmult -iexamples/lib -fsimplifier-phases=3 -package dph-base

# -----------------
fft_DPH		= fft
fft_SOURCES	= \
	examples/fft/Main.hs \
	examples/fft/FFTCArray.hs

fft_seq_FLAGS = -iexamples/fft
fft_par_FLAGS = -iexamples/fft -rtsopts

include $(TOPDIR)/mk/test.mk
