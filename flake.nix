{
  description = "Flake for One Student One Chip Project";
  inputs = {
    mill01214pkgs.url = "github:NixOS/nixpkgs/4907750a173268bf52f55bd16f3669bf2edeac30";
    pkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };
  outputs = {
    self,
    pkgs,
    mill01214pkgs,
  }: let
    stdpkgs = pkgs.legacyPackages.x86_64-linux;
    npcmake = stdpkgs.writeScriptBin "npcmake" ''make -C $NPC_HOME $1'';
    nemumake = stdpkgs.writeScriptBin "nemumake" ''make -C $NEMU_HOME $1'';
    ista-run = stdpkgs.writeScriptBin "ista-run" ''LD_LIBRARY_PATH=bin/ ista-bin'';
    riscv-toolchain = import pkgs {
      localSystem = "x86_64-linux";
      crossSystem = {
        config = "riscv64-unknown-linux-gnu";
        gcc = {abi = "ilp32";};
      };
    };
  in rec {
    formatter.x86_64-linux = pkgs.legacyPackages.x86_64-linux.alejandra;

    packages.x86_64-linux.espresso = pkgs.legacyPackages.x86_64-linux.stdenv.mkDerivation rec {
      pname = "espresso";
      version = "2.4";
      nativeBuildInputs = [stdpkgs.cmake stdpkgs.ninja];
      src = stdpkgs.fetchFromGitHub {
        owner = "chipsalliance";
        repo = "espresso";
        rev = "v${version}";
        sha256 = "sha256-z5By57VbmIt4sgRgvECnLbZklnDDWUA6fyvWVyXUzsI=";
      };
    };

    devShells.x86_64-linux.default = pkgs.legacyPackages.x86_64-linux.mkShell {
      name = "osoc-shell";
      packages = [
        stdpkgs.verilator
        stdpkgs.gtkwave
        stdpkgs.surfer
        stdpkgs.bison
        stdpkgs.dtc
        stdpkgs.flex
        stdpkgs.ncurses
        stdpkgs.readline
        stdpkgs.llvm
        stdpkgs.gdb
        stdpkgs.perf
        stdpkgs.julia-bin
        stdpkgs.python3
        stdpkgs.perl
        stdpkgs.libunwind
        stdpkgs.yosys
        stdpkgs.sby
        stdpkgs.z3
        stdpkgs.yosys-synlig
        stdpkgs.surelog
        stdpkgs.verible
        stdpkgs.metals
        stdpkgs.taplo
        stdpkgs.ieda
        npcmake
        nemumake
        ista-run
      ];

      nativeBuildInputs = [
        stdpkgs.pkg-config
        stdpkgs.rustPlatform.bindgenHook
      ];

      buildInputs = [
        stdpkgs.clang-tools
        stdpkgs.capstone
        stdpkgs.tomlplusplus
        stdpkgs.sqlite
        stdpkgs.gnumake
        stdpkgs.just
        stdpkgs.scalafmt
        stdpkgs.scalafix
        (mill01214pkgs.legacyPackages."x86_64-linux".mill.override {jre = stdpkgs.temurin-jre-bin-17;}) # scala project builder
        (stdpkgs.sbt.override {jre = stdpkgs.temurin-jre-bin-17;}) # scala project builder
        stdpkgs.temurin-bin
        stdpkgs.circt
        packages.x86_64-linux.espresso
        stdpkgs.scons
        stdpkgs.bear
        riscv-toolchain.buildPackages.gcc
        stdpkgs.SDL2
        stdpkgs.SDL2_image
        stdpkgs.SDL2_ttf
        stdpkgs.SDL # Required by flappy bird sdl
        stdpkgs.SDL_image # Required by flappy bird sdl
      ];

      shellHook = ''
        export NPC_HOME=`readlink -f npc`
        export NPC_CHISEL=`readlink -f npc-chisel`
        export YSYX_SOC_HOME=`readlink -f ysyxSoC`
        export NEMU_HOME=`readlink -f nemu`
        export AM_HOME=`readlink -f abstract-machine`
        export NAVY_HOME=`readlink -f navy-apps`
        export NVBOARD_HOME=`readlink -f nvboard`
        export LAB_HOME=`readlink -f digital-design-lab`
        export YOSYS_STA_HOME=`readlink -f yosys-sta`
        export RUSTUP_HOME="$HOME/.rustup"
        export PATH="$RUSTUP_HOME/stable-${stdpkgs.stdenv.hostPlatform.rust.rustcTarget}/bin:$NPC_CHISEL/micro-sim/target/release:$NPC_CHISEL/out/bin:$NPC_HOME/build/bin:$PATH"
        export CHISEL_FIRTOOL_PATH=${stdpkgs.circt}/bin
        export NIX_CFLAGS_COMPILE="$(pkg-config --cflags sdl2) $(pkg-config --cflags verilator) $NIX_CFLAGS_COMPILE"
        export CPATH="$(pkg-config --cflags-only-I verilator | sed 's/ -I/:/' | sed 's/^..//'):$(readlink -f npc)/build:$NVBOARD_HOME/usr/include"
        alias npcmake="make -C $NPC_HOME"
      '';
    };
  };
}
