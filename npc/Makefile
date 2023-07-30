BUILD_DIR=$(NPC_HOME)/build
EMBEDSRC_DIR=$(NPC_HOME)/examples/embeddedtour
EMBEDSRC=$(shell find $(EMBEDSRC_DIR) -name '*.cpp' -or -name '*.v' -or -name '*.sv')
DLCOLABSRC_DIR=$(wildcard $(NPC_HOME)/examples/njudlcolab/0*)
VERILATOR=verilator

all:
	@echo "Write this Makefile by your self."

build build/bin:
	mkdir -p $@

define run-dlcobuild =
@echo $(subst 0,nju,$(notdir $(filedir)))
$(VERILATOR) --cc -Mdir $(BUILD_DIR) --top-module $(subst 0,,$(notdir $(filedir))) \
	--build -Wall $(shell find $(filedir) -name '*.cpp' -or -name '*.v' -or -name '*.sv') $(NVBOARD_ARCHIVE) \
	-LDFLAGS -lSDL2 -LDFLAGS -lSDL2_image \
	--exe -o $(BUILD_DIR)/bin/$(subst 0,nju,$(notdir $(filedir)));
endef

njudlco: build build/bin $(DLCOLABSRC_DIR)
	$(foreach filedir,$(DLCOLABSRC_DIR),$(run-dlcobuild))
	$(call git_commit, "Build NJU DLCO LAB files.")

embed: buildembed
	$(call git_commit, "Build itsembedded tutorial codes.")
	@echo "Building Makefile..."

buildembed: $(EMBEDSRC)
	$(VERILATOR) --cc --Mdir $(BUILD_DIR) $(EMBEDSRC)

sim:
	$(call git_commit, "sim RTL") # DO NOT REMOVE THIS LINE!!!
	@echo "Write this Makefile by your self."
	verilator --cc --exe --build -j 0 -Wall vsrc/example/01_xor_light/sim_main.cpp vsrc/example/01_xor_light/xormod.v

.PHONY: sim clean embed njudlco

clean:
	rm -r $(BUILD_DIR)
include ../Makefile
include $(NVBOARD_HOME)/scripts/nvboard.mk