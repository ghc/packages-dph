

dph-test/bin/war : $(shell find dph-test/framework -name "*.hs")
	@echo "* Build war test driver"
	@$(GHC_FRAMEWORK) \
		-threaded \
		-package stm \
		-idph-test/framework --make dph-test/framework/Main.hs -o dph-test/bin/war -threaded
	@echo

.PHONY : test-prims
test-prims : dph-test/bin/war
	@echo "* Running tests"
	@dph-test/bin/war -d dph-test/test
	@echo 