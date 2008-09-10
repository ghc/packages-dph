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

ifeq ($($(BINARY)_DPH),prim)
WAY_FLAGS = -package dph-prim-$(WAY) -odir $(WAY) -hidir $(WAY)
else
WAY_FLAGS = -fdph-$(WAY) -package dph-$(WAY) -odir $(WAY) -hidir $(WAY)
endif

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

