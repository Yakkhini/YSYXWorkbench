all:
	@echo "Write this Makefile by your self."

sim:
	$(call git_commit, "sim RTL") # DO NOT REMOVE THIS LINE!!!
	@echo "Write this Makefile by your self."
	verilator --cc --exe --build -j 0 -Wall vsrc/example/01_xor_light/sim_main.cpp vsrc/example/01_xor_light/xormod.v

include ../Makefile
