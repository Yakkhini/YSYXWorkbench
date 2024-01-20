NPC_NAME=sriz
BUILD_DIR=$(NPC_HOME)/build
EMBEDSRC_DIR=$(NPC_HOME)/examples/embeddedtour
EMBEDSRC=$(shell find $(EMBEDSRC_DIR) -name '*.cpp' -or -name '*.v' -or -name '*.sv')
DLCOLABSRC_DIR=$(wildcard $(NPC_HOME)/examples/njudlcolab/0*)
YSYXPRESRC_DIR=$(wildcard $(NPC_HOME)/examples/ysyxpre/0*)
CSRC_DIR=$(NPC_HOME)/csrc
VSRC_DIR=$(NPC_HOME)/vsrc
INC_PATH=$(NPC_HOME)/include
CSRC=$(shell find $(CSRC_DIR) -name '*.cpp')
VSRC=$(shell find $(VSRC_DIR) -name '*.sv')
VERILATOR=verilator

export CPATH := $(INC_PATH):$(CPATH)

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

compile: build/bin $(CSRC) $(VSRC)
	$(call git_commit, "sim RTL") # DO NOT REMOVE THIS LINE!!!
	@echo "New Processor Core Sim RTL."
	$(VERILATOR) --cc --Mdir $(BUILD_DIR) --top-module $(NPC_NAME) \
	    --build -Wall -Wno-DECLFILENAME -Wno-UNUSEDSIGNAL -Wno-UNDRIVEN $(CSRC) $(VSRC) \
		--trace --exe -o $(BUILD_DIR)/bin/$(NPC_NAME)

sim: compile
	$(call git_commit, "sim RTL") # DO NOT REMOVE THIS LINE!!!
	@echo "New Processor Core Sim RTL."
	sriz $(IMG)

wave: build/waveform.vcd
	gtkwave $(NPC_HOME)/build/waveform.vcd

build/waveform.vcd: compile
	$(NPC_HOME)/build/bin/sriz

fmt: $(CSRC) $(VSRC)
	clang-format -i $(CSRC) $(shell find $(NPC_HOME)/include -name '*.h')
	verible-verilog-format --inplace $(VSRC)

clean:
	rm -rf $(BUILD_DIR)
	rm -rf $(YOSYS_STA_HOME)/result

.PHONY: compile sim wave fmt clean embed njudlco ysyxpre

include ../Makefile
include $(NVBOARD_HOME)/scripts/nvboard.mk