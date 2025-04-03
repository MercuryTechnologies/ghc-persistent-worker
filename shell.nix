let
  pkgs = import (fetchTarball {
    url = "https://github.com/nixos/nixpkgs/archive/e913ae340076bbb73d9f4d3d065c2bca7caafb16.tar.gz";
    sha256 = "07qjibn85hc6p8c7lwg00fwpyqjlmaxncz9wa2l6qpy1hsk51k8f";
  }) {};
in pkgs.mkShell {
  ghc_dir = "${pkgs.haskell.compiler.ghc910}";
  packages = [pkgs.haskell.compiler.ghc910 pkgs.zlib pkgs.zlib.dev pkgs.snappy pkgs.protobuf];
}
