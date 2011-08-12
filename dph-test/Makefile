

all : test

bin/war : $(shell find war -name "*.hs")
	ghc -iwar --make war/Main.hs -o bin/war -threaded
	
.PHONY : test
test : bin/war
	bin/war -d test
	
clean :
	@echo "Cleaning..."
	@rm -f bin/*
	@find . -name "*.hi" | xargs rm
	