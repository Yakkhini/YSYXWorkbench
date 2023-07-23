{
  description = "Flake for ysyx";
  inputs = {
    verilator5008pkgs.url = "github:NixOS/nixpkgs/5e871533c4a488319b9cb98d7a525c356459d36c";
    pkgs.url = "pkgs";
  };
  outputs = {
    self,
    pkgs,
    verilator5008pkgs,
  }: let
    stdpkgs = pkgs.legacyPackages.x86_64-linux;
  in {
    formatter.x86_64-linux = pkgs.legacyPackages.x86_64-linux.alejandra;
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
      ];

      buildInputs = with stdpkgs; [
        clang-tools
        gnumake
        bear
        SDL2
        SDL2_image
        pkg-config
      ];

      shellHook = ''
        export NPC_HOME=`readlink -f npc`
        export NEMU_HOME=`readlink -f nemu`
        export AM_HOME=`readlink -f abstract-machine`
        export NVBOARD_HOME=`readlink -f nvboard`
        export NIX_CFLAGS_COMPILE="$(pkg-config --cflags sdl2) $(pkg-config --cflags verilator) $NIX_CFLAGS_COMPILE"
        export CPATH="$(pkg-config --cflags-only-I verilator | sed 's/ -I/:/' | sed 's/^..//')"
      '';
    };
  };
}
