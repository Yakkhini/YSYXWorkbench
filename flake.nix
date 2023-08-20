{
  description = "Flake for ysyx";
  inputs = {
    verilator5008pkgs.url = "github:NixOS/nixpkgs/5e871533c4a488319b9cb98d7a525c356459d36c";
    yamlcpp06pkgs.url = "github:NixOS/nixpkgs/66e44425c6dfecbea68a5d6dc221ccd56561d4f1";
    pkgsunstable.url = "github:NixOS/nixpkgs/nixos-unstable";
    pkgs.url = "pkgs";
  };
  outputs = {
    self,
    pkgs,
    pkgsunstable,
    verilator5008pkgs,
    yamlcpp06pkgs,
  }: let
    stdpkgs = pkgs.legacyPackages.x86_64-linux;
    npcmake = stdpkgs.writeScriptBin "npcmake" ''make -C $NPC_HOME $1'';
  in rec {
    formatter.x86_64-linux = pkgs.legacyPackages.x86_64-linux.alejandra;
    packages.x86_64-linux.ista-bin = pkgs.legacyPackages.x86_64-linux.stdenv.mkDerivation {
      name = "ista-bin";

      system = "x86_64-linux";

      src = ./npc/bin/iSTA;
      nativeBuildInputs = [
        stdpkgs.autoPatchelfHook # Automatically setup the loader, and do the magic
      ];

      # Required at running time
      buildInputs = [
        stdpkgs.glibc
        stdpkgs.libunwind
        stdpkgs.zlib
        stdpkgs.tcllib
        yamlcpp06pkgs.legacyPackages.x86_64-linux.libyamlcpp
      ];

      unpackPhase = "true";

      # Extract and copy executable in $out/bin
      installPhase = ''
        mkdir -p $out/bin
        cp $src $out/bin/ista-bin
      '';

      meta = with stdpkgs.lib; {
        description = "iSTA binary";
        homepage = https://github.com/OSCC-Project/iEDA;
        license = licenses.mulan-psl2;
        maintainers = with stdenv.lib.maintainers; [YAKKHINI];
        platforms = ["x86_64-linux"];
      };
    };
    devShells.x86_64-linux.default = pkgs.legacyPackages.x86_64-linux.mkShell {
      name = "ysyx-shell";
      packages = [
        verilator5008pkgs.legacyPackages.x86_64-linux.verilator
        stdpkgs.gtkwave
        stdpkgs.bison
        stdpkgs.flex
        stdpkgs.ncurses
        stdpkgs.readline
        stdpkgs.llvm
        stdpkgs.gdb
        stdpkgs.python3
        stdpkgs.perl
        stdpkgs.libunwind
        stdpkgs.yosys
        packages.x86_64-linux.ista-bin
        npcmake
      ];

      buildInputs = [
        stdpkgs.clang-tools
        stdpkgs.gnumake
        stdpkgs.bear
        pkgsunstable.legacyPackages.x86_64-linux.pkgsCross.riscv64-embedded.buildPackages.gcc
        pkgsunstable.legacyPackages.x86_64-linux.pkgsCross.riscv32-embedded.libcCross
        stdpkgs.SDL2
        stdpkgs.SDL2_image
        stdpkgs.pkg-config
      ];

      shellHook = ''
        export NPC_HOME=`readlink -f npc`
        export NEMU_HOME=`readlink -f nemu`
        export AM_HOME=`readlink -f abstract-machine`
        export NVBOARD_HOME=`readlink -f nvboard`
        export YOSYS_STA_HOME=`readlink -f yosys-sta`
        export PATH="$NPC_HOME/build/bin:$PATH"
        export NIX_CFLAGS_COMPILE="$(pkg-config --cflags sdl2) $(pkg-config --cflags verilator) $NIX_CFLAGS_COMPILE"
        export CPATH="$(pkg-config --cflags-only-I verilator | sed 's/ -I/:/' | sed 's/^..//'):$(readlink -f npc)/build:$NVBOARD_HOME/include"
        alias npcmake="make -C $NPC_HOME"
      '';
    };
  };
}
