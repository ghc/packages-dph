# Makes the dph-examples.cabal file from its template.

# Package dependencies for DPH examples.
DPH_DEPENDS_PAR = \
        base         == 4.4.*, \
        vector       == 0.7.*, \
        dph-base     == 0.5.*, \
        dph-prim-par == 0.5.*, \
        dph-par      == 0.5.*, \
        random       == 1.0.*, \
        old-time     == 1.0.*

DPH_DEPENDS_SEQ = \
        base         == 4.4.*, \
        vector       == 0.7.*, \
        dph-base     == 0.5.*, \
        dph-prim-seq == 0.5.*, \
        dph-seq      == 0.5.*, \
        random       == 1.0.*, \
        old-time     == 1.0.*


# Options for compiling DPH examples.
# 	For the SEQ versions we still compile with -threaded so that we can 
# 	use the parallel garbage collector.
#
DPH_OPTIONS_PAR = \
        -rtsopts \
        -threaded \
	-fllvm \
        -Odph \
        -fdph-par \
        -fcpr-off \
        -fsimplifier-phases=4 \
        -fstrictness-before=3 

DPH_OPTIONS_SEQ = \
        -rtsopts \
	-threaded \
	-fllvm \
        -Odph \
        -fdph-seq \
        -fcpr-off \
        -fsimplifier-phases=4 \
        -fstrictness-before=3 

.PHONY : all
all : dph-examples.cabal
	cabal configure --user
	cabal build

# Make the cabal file from the tempalate by substituting in the above config.
dph-examples.cabal : dph-examples.template Makefile
	@echo "* Making dph-examples.cabal"
	@cpp 	-P -undef \
		-DDPH_DEPENDS_PAR="$(DPH_DEPENDS_PAR)" \
		-DDPH_DEPENDS_SEQ="$(DPH_DEPENDS_SEQ)" \
		-DDPH_OPTIONS_PAR="$(DPH_OPTIONS_PAR)" \
		-DDPH_OPTIONS_SEQ="$(DPH_OPTIONS_SEQ)" \
		dph-examples.template dph-examples.cabal 
	