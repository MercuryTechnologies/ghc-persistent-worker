{
  description = "A flake for ghc-persistent-worker";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };

      in
      {
        devShells.default =
          let
            libs = [
              pkgs.zstd
              pkgs.xz
              pkgs.bzip2
              pkgs.zlib
            ];
          in
          pkgs.mkShell {
            ghc_dir = "${pkgs.haskell.compiler.ghc910}";

            packages = [
              (pkgs.haskell.packages.ghc910.ghcWithPackages (g: [ g.cabal-install ]))
              pkgs.zlib.dev
              pkgs.snappy
              pkgs.protobuf
              pkgs.git
              pkgs.nixfmt-rfc-style
            ] ++ libs;

            shellHook = ''
              # prompt
              export PS1="\n[ghc-persistent-worker:\w]$ \0"
              # dynamic library path
              export LD_LIBRARY_PATH="${pkgs.lib.makeSearchPathOutput "out" "lib" libs}:$LD_LIBRARY_PATH"
            '';
          };
      }
    );
}
