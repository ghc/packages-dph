WAYS = seq par

AUTO_BINARY = $(notdir $@)
AUTO_WAY  = $(patsubst %/,%,$(dir $@))

DPH_BINARIES = $(foreach binary, $(BINARIES), $(if $($(binary)_DPH), $(foreach way, $(WAYS), $(way)/$(binary))))

include $(TOPDIR)/mk/common.mk

ifeq (,$(BINARY))
.PHONY: all $(DPH_BINARIES) force

all: $(DPH_BINARIES)

$(DPH_BINARIES): force
	$(MAKE) BINARY=$(AUTO_BINARY) WAY=$(AUTO_WAY)

force:
else

WAY_FLAGS = -fdph-$(WAY) -odir $(WAY) -hidir $(WAY)

$(WAY)/$(BINARY): $($(BINARY)_SOURCES) $(BENCH_DEP)
	@mkdir $(WAY) || true
	$(HC) -o $@ --make $< $(DPH_FLAGS) $(WAY_FLAGS) $(BENCH_FLAGS)

endif

.PHONY: clean

clean:
	$(RM) -r $(WAYS)

.PHONY: bench

$(BENCH_DEP):
	cd $(BENCH_DIR) && $(MAKE)

