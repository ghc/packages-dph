
.PHONY	: clean
clean :
	@echo "* Cleaning up"
	@rm -f make/Makefile.deps
	@find . \
			-name "*.deps" \
		-o      -name "*.deps.inc" \
		-o      -name "*.deps.bak" \
		-o	-name "*.o" \
		-o      -name "*.o-boot" \
		-o	-name "*.hi" \
		-o	-name "*.hi-boot" \
		-follow | xargs -n 1 rm -f
	@echo
