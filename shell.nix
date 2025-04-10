let
  pkgs = import (fetchTarball {
    url = "https://github.com/nixos/nixpkgs/archive/ba487dbc9d04e0634c64e3b1f0d25839a0a68246.tar.gz";
    sha256 = "0wr8pnx2bkr88vxv3aqa9y9vrcixicm2vahws7i2kvcpy8mnb4sr";
  }) {};

  libs = [
    pkgs.zstd
    pkgs.xz
    pkgs.bzip2
    pkgs.zlib
  ];


in pkgs.mkShell {

  ghc_dir = "${pkgs.haskell.compiler.ghc910}";

  packages = [
    (pkgs.haskell.packages.ghc910.ghcWithPackages (g: [g.cabal-install]))
    pkgs.zlib.dev
    pkgs.snappy
    pkgs.protobuf
    pkgs.git
  ] ++ libs;

  shellHook = ''
  export LD_LIBRARY_PATH="${pkgs.lib.makeSearchPathOutput "out" "lib" libs}:$LD_LIBRARY_PATH"
  '';

}
