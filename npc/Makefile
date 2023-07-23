BUILD_DIR := ./build
EMBEDSRC_DIR := ./examples/embeddedtour
EMBEDSRC := $(shell find $(SRC_DIRS) -name '0*_*.cpp' -or -name '0*_*.v' -or -name '0*_*.sv')
VERILATOR := verilator

all:
	@echo "Write this Makefile by your self."

embed: buildembed
	$(call git_commit, "Build itsembedded tutorial codes.")
	@echo "Building Makefile..."

buildembed: $(EMBEDSRC)
	$(VERILATOR) --cc --Mdir $(BUILD_DIR) $(EMBEDSRC)

sim:
	$(call git_commit, "sim RTL") # DO NOT REMOVE THIS LINE!!!
	@echo "Write this Makefile by your self."
	verilator --cc --exe --build -j 0 -Wall vsrc/example/01_xor_light/sim_main.cpp vsrc/example/01_xor_light/xormod.v

.PHONY: sim clean embed

clean:
	rm -r $(BUILD_DIR)
include ../Makefile
