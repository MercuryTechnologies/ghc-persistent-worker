let
  pkgs = import (fetchTarball {
    url = "https://github.com/nixos/nixpkgs/archive/ba487dbc9d04e0634c64e3b1f0d25839a0a68246.tar.gz";
    sha256 = "0wr8pnx2bkr88vxv3aqa9y9vrcixicm2vahws7i2kvcpy8mnb4sr";
  }) {};
in pkgs.mkShell {
  ghc_dir = "${pkgs.haskell.compiler.ghc910}";
  packages = [
    pkgs.haskell.compiler.ghc910
    pkgs.haskell.packages.ghc910.cabal-install
    pkgs.zlib
    pkgs.zlib.dev
    pkgs.zstd
    pkgs.snappy
    pkgs.protobuf
    pkgs.git
  ];
}
