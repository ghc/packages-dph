WAYS = seq par

AUTO_BINARY = $(notdir $@)
AUTO_WAY  = $(patsubst %/,%,$(dir $@))

DPH_PRE_BINARIES = $(foreach binary, $(BINARIES), $(if $($(binary)_DPH), $(binary)))
DPH_BINARIES = $(foreach binary, $(DPH_PRE_BINARIES), $(foreach way, $(WAYS), $(way)/$(binary)))

OTHER_BINARIES = $(foreach binary, $(BINARIES), $(if $($(binary)_DPH), , $(binary)))

include $(TOPDIR)/mk/common.mk

ifeq (,$(BINARY))
.PHONY: all $(DPH_BINARIES) $(OTHER_BINARIES) force

all: $(DPH_BINARIES) $(OTHER_BINARIES)

$(DPH_BINARIES): force
	$(MAKE) BINARY=$(AUTO_BINARY) WAY=$(AUTO_WAY)

$(OTHER_BINARIES): force
	$(MAKE) BINARY=$@ WAY=other

force:
else

ifeq ($(WAY),other)
flags = $(BINARY)_FLAGS
else
flags = $(BINARY)_$(WAY)_FLAGS
$(flags) += -fdph-$(WAY)
endif

$(flags) += -odir $(WAY) -hidir $(WAY)

$(WAY)/$(BINARY): $($(BINARY)_SOURCES) $(BENCH_DEP)
	@mkdir $(WAY) || true
	$(HC) -o $@ --make $< $(DPH_FLAGS) $($(flags)) $(BENCH_FLAGS)
endif

.PHONY: clean

clean:
	$(RM) -r $(WAYS)

.PHONY: bench

$(BENCH_DEP):
	cd $(BENCH_DIR) && $(MAKE)

