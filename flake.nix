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
    in {
      devShells.default = import ./shell.nix {inherit system pkgs;};
    });
}
