BUILD_DIR=$(NPC_HOME)/build
EMBEDSRC_DIR=$(NPC_HOME)/examples/embeddedtour
EMBEDSRC=$(shell find $(EMBEDSRC_DIR) -name '*.cpp' -or -name '*.v' -or -name '*.sv')
DLCOLABSRC_DIR=$(wildcard $(NPC_HOME)/examples/njudlcolab/0*)
YSYXPRESRC_DIR=$(wildcard $(NPC_HOME)/examples/ysyxpre/0*)
VERILATOR=verilator

all:
	@echo "Write this Makefile by your self."

build/bin:
	mkdir -p $@

define run-dlcobuild =
@echo $(subst 0,nju,$(notdir $(filedir)))
$(VERILATOR) --cc -Mdir $(BUILD_DIR) --top-module $(subst 0,,$(notdir $(filedir))) \
	--build -Wall $(shell find $(filedir) -name '*.cpp' -or -name '*.v' -or -name '*.sv') $(NVBOARD_ARCHIVE) \
	-LDFLAGS -lSDL2 -LDFLAGS -lSDL2_image \
	--exe -o $(BUILD_DIR)/bin/$(subst 0,nju,$(notdir $(filedir)))
cd $(YOSYS_STA_HOME)
make -C $(YOSYS_STA_HOME) sta DESIGN=$(subst 0,,$(notdir $(filedir))) RTL_FILES="$(shell find $(filedir) -name '*.v')" CLK_FREQ_MHZ=100
cd $(NPC_HOME)

endef

njudlco: build/bin $(DLCOLABSRC_DIR)
	$(foreach filedir,$(DLCOLABSRC_DIR),$(run-dlcobuild))
	cp -r $(YOSYS_STA_HOME)/result $(NPC_HOME)/build/result
	$(call git_commit, "Build NJU DLCO LAB files.")

define run-ysyxprebuild =
@echo $(subst 0,ysyxpre,$(notdir $(filedir)))
$(VERILATOR) --cc -Mdir $(BUILD_DIR) --top-module $(subst 0,,$(notdir $(filedir))) \
	--build -Wall $(shell find $(filedir) -name '*.cpp' -or -name '*.v' -or -name '*.sv') $(NVBOARD_ARCHIVE) \
	-LDFLAGS -lSDL2 -LDFLAGS -lSDL2_image \
	--exe -o $(BUILD_DIR)/bin/$(subst 0,ysyxpre,$(notdir $(filedir)))

endef

ysyxpre: build/bin $(YSYXPRESRC_DIR)
	$(foreach filedir,$(YSYXPRESRC_DIR),$(run-ysyxprebuild))
	$(call git_commit, "Build YSYX prestudy files.")

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
	rm -r $(YOSYS_STA_HOME)/result
include ../Makefile
include $(NVBOARD_HOME)/scripts/nvboard.mk