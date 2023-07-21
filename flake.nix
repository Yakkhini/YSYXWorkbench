{
  description = "Flake for ysyx";
  inputs = {
    verilator5002pkgs.url = "github:NixOS/nixpkgs/3c3b3ab88a34ff8026fc69cb78febb9ec9aedb16";
    pkgs.url = "pkgs";
  };
  outputs = {
    self,
    pkgs,
    verilator5002pkgs,
  }: let
    stdpkgs = pkgs.legacyPackages.x86_64-linux;
  in {
    formatter.x86_64-linux = pkgs.legacyPackages.x86_64-linux.alejandra;
    devShells.x86_64-linux.default = pkgs.legacyPackages.x86_64-linux.mkShell {
      name = "ysyx-shell";
      packages = [
        verilator5002pkgs.legacyPackages.x86_64-linux.verilator
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
        gnumake
        SDL2
        SDL2_image
        pkg-config
      ];

      shellHook = ''
        export NPC_HOME=`readlink -f npc`
        export NEMU_HOME=`readlink -f nemu`
        export AM_HOME=`readlink -f abstract-machine`
        export NVBOARD_HOME=`readlink -f nvboard`
        NIX_CFLAGS_COMPILE="$(pkg-config --cflags sdl2) $NIX_CFLAGS_COMPILE"
      '';
    };
  };
}
