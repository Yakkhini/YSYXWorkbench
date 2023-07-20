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
  }: {
    formatter.x86_64-linux = pkgs.legacyPackages.x86_64-linux.alejandra;
    devShells.x86_64-linux.default = pkgs.legacyPackages.x86_64-linux.mkShell {
      packages = [
        verilator5002pkgs.legacyPackages.x86_64-linux.verilator
        pkgs.legacyPackages.x86_64-linux.gtkwave
      ];
      shellHook = ''
        export NPC_HOME=`readlink -f npc`
        export NEMU_HOME=`readlink -f nemu`
        export AM_HOME=`readlink -f abstract-machine`
      '';
    };
  };
}
