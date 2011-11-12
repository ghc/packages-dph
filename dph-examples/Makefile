# Makes the dph-examples.cabal file from its template.

# Package dependencies for DPH examples.
DPH_DEPENDS = \
        base         	== 4.4.*, \
        vector       	== 0.9.*, \
        random       	== 1.0.*, \
        old-time     	== 1.0.*, \
        dph-base     	== 0.5.*, \
        dph-prim-par	== 0.5.*, \
        dph-lifted-vseg	== 0.5.*


# Options for compiling DPH examples.
DPH_OPTIONS= \
        -rtsopts \
        -threaded \
	-fllvm \
        -Odph \
        -package dph-lifted-vseg \
        -fcpr-off \
	-fno-liberate-case \
	-fno-spec-constr \
        -fsimpl-tick-factor=1000 


.PHONY : all
all : dph-examples.cabal
	cabal configure --user
	cabal build


# Make the cabal file from the tempalate by substituting in the above config.
dph-examples.cabal : dph-examples.template Makefile
	@echo "* Making dph-examples.cabal"
	@cpp 	-P -undef \
		-DDPH_DEPENDS="$(DPH_DEPENDS)" \
		-DDPH_OPTIONS="$(DPH_OPTIONS)" \
		dph-examples.template dph-examples.cabal 
	