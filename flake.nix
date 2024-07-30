{
  description = "ghc-worker";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/master";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = {
    self,
    nixpkgs,
    flake-utils,
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = import nixpkgs {inherit system;};
      hpkgs = pkgs.haskellPackages;
      hsenv = hpkgs.ghcWithPackages (p: with p; [
        network
      ]);
    in {
      devShells.default = pkgs.mkShell {
        name = "ghc-worker-test-shell";
        buildInputs = [
          hsenv
        ];
        shellHook = ''
          export PS1="\n[ghc-worker-test-shell:\w]$ \0"
        '';
      };
    });
}
