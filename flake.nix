{
  description = "Flake for ysyx";
  inputs = {
    yamlcpp06pkgs.url = "github:NixOS/nixpkgs/66e44425c6dfecbea68a5d6dc221ccd56561d4f1";
    pkgsunstable.url = "github:NixOS/nixpkgs/nixos-unstable";
    pkgs.url = "pkgs";
  };
  outputs = {
    self,
    pkgs,
    pkgsunstable,
    yamlcpp06pkgs,
  }: let
    stdpkgs = pkgs.legacyPackages.x86_64-linux;
    npcmake = stdpkgs.writeScriptBin "npcmake" ''make -C $NPC_HOME $1'';
    ista-run = stdpkgs.writeScriptBin "ista-run" ''LD_LIBRARY_PATH=bin/ ista-bin'';
  in rec {
    formatter.x86_64-linux = pkgs.legacyPackages.x86_64-linux.alejandra;
    packages.x86_64-linux.ista-bin = pkgs.legacyPackages.x86_64-linux.stdenv.mkDerivation {
      name = "ista-bin";

      system = "x86_64-linux";

      src = stdpkgs.fetchzip {
        url = "https://ysyx.oscc.cc/slides/resources/archive/ista.tar.bz2";
        hash = "sha256-yseeHz+lVA+q9K2A40iNUP6jf/sGYjqKwga5gLvaXYo=";
      };
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
        cp $src/iSTA $out/bin/ista-bin
      '';

      meta = with stdpkgs.lib; {
        description = "iSTA binary";
        homepage = https://github.com/OSCC-Project/iEDA;
        license = licenses.mulan-psl2;
        maintainers = with stdenv.lib.maintainers; [YAKKHINI];
        platforms = ["x86_64-linux"];
      };
    };

    packages.x86_64-linux.capstone = pkgs.legacyPackages.x86_64-linux.stdenv.mkDerivation rec {
      pname = "capstone";
      version = "5.0.1";

      src = stdpkgs.fetchFromGitHub {
        owner = "capstone-engine";
        repo = "capstone";
        rev = version;
        sha256 = "sha256-kKmL5sae9ruWGu1gas1mel9qM52qQOD+zLj8cRE3isg=";
      };

      nativeBuildInputs =
        [
          stdpkgs.cmake
        ]
        ++ stdpkgs.lib.optionals stdpkgs.stdenv.isDarwin [
          stdpkgs.lib.fixDarwinDylibNames
        ];

      doCheck = true;

      meta = with stdpkgs.lib; {
        description = "Advanced disassembly library";
        homepage = "http://www.capstone-engine.org";
        license = licenses.bsd3;
        maintainers = with stdenv.lib.maintainers; [thoughtpolice ris];
        mainProgram = "cstool";
        platforms = ["x86_64-linux"];
      };
    };

    devShells.x86_64-linux.default = pkgs.legacyPackages.x86_64-linux.mkShell {
      name = "ysyx-shell";
      packages = [
        stdpkgs.verilator
        stdpkgs.gtkwave
        stdpkgs.bison
        stdpkgs.dtc
        stdpkgs.flex
        stdpkgs.ncurses
        stdpkgs.readline
        stdpkgs.llvm
        stdpkgs.gdb
        stdpkgs.python3
        stdpkgs.perl
        stdpkgs.libunwind
        stdpkgs.yosys
        stdpkgs.yosys-synlig
        stdpkgs.surelog
        stdpkgs.verible
        packages.x86_64-linux.ista-bin
        packages.x86_64-linux.capstone
        npcmake
        ista-run
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
        export NAVY_HOME=`readlink -f navy-apps`
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
