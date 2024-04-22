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
    nemumake = stdpkgs.writeScriptBin "nemumake" ''make -C $NEMU_HOME $1'';
    ista-run = stdpkgs.writeScriptBin "ista-run" ''LD_LIBRARY_PATH=bin/ ista-bin'';
    riscv-toolchain = import pkgsunstable {
      localSystem = "x86_64-linux";
      crossSystem = {
        config = "riscv64-unknown-linux-gnu";
        gcc = {abi = "ilp32";};
      };
    };
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
        homepage = "https://github.com/OSCC-Project/iEDA";
        license = licenses.mulan-psl2;
        maintainers = with stdenv.lib.maintainers; [YAKKHINI];
        platforms = ["x86_64-linux"];
      };
    };

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
        stdpkgs.metals # scala lsp
        packages.x86_64-linux.ista-bin
        packages.x86_64-linux.capstone
        npcmake
        nemumake
        ista-run
      ];

      buildInputs = [
        stdpkgs.clang-tools
        stdpkgs.gnumake
        stdpkgs.just
        stdpkgs.scalafmt
        (stdpkgs.mill.override {jre = stdpkgs.temurin-bin;}) # scala project builder
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
        stdpkgs.pkg-config
      ];

      shellHook = ''
        export NPC_HOME=`readlink -f npc`
        export NPC_CHISEL=`readlink -f npc-chisel`
        export NEMU_HOME=`readlink -f nemu`
        export AM_HOME=`readlink -f abstract-machine`
        export NAVY_HOME=`readlink -f navy-apps`
        export NVBOARD_HOME=`readlink -f nvboard`
        export LAB_HOME=`readlink -f digital-design-lab`
        export YOSYS_STA_HOME=`readlink -f yosys-sta`
        export PATH="$NPC_CHISEL/out/bin:$NPC_HOME/build/bin:$PATH"
        export CHISEL_FIRTOOL_PATH=${stdpkgs.circt}/bin
        export NIX_CFLAGS_COMPILE="$(pkg-config --cflags sdl2) $(pkg-config --cflags verilator) $NIX_CFLAGS_COMPILE"
        export CPATH="$(pkg-config --cflags-only-I verilator | sed 's/ -I/:/' | sed 's/^..//'):$(readlink -f npc)/build:$NVBOARD_HOME/include"
        alias npcmake="make -C $NPC_HOME"
      '';
    };
  };
}
