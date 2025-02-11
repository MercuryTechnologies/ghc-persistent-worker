{
  description = "GHC server";
  inputs = {
    nixpkgs-old.url = "github:MercuryTechnologies/nixpkgs/ghc962";
    nixpkgs.url = "github:nixos/nixpkgs/1276b88d5c577097e8972f69816fc39ed090737c";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = {nixpkgs-old, nixpkgs, flake-utils, ...}:
    flake-utils.lib.eachDefaultSystem (system: let

      pkgs = import nixpkgs {
        inherit system;
        config = {
          allowUnfree = true;
          allowBroken = true;
        };
      };

      pkgsOld = import nixpkgs-old {
        inherit system;
        config = {
          allowUnfree = true;
          allowBroken = true;
        };
      };

      buckWorker = pkgs.writeScript "buck-worker" ''
      #!/bin/sh
      export WORKER_SOCKET=$PWD/pro/worker-socket
      rm -f $WORKER_SOCKET
      exec nix develop -c cabal run buck-worker:exe:worker
      '';

      test-client = import ./nix/test-client.nix { inherit pkgs; };

    in {

      devShells = {
        default = pkgs.mkShell {
          packages = [
            (pkgs.haskell.packages.ghc910.ghcWithPackages (g: [g.cabal-install g.haskell-language-server]))
            # newer grpc has breaking changes
            pkgsOld.grpc
            pkgs.grpcurl
          ];
          shellHook = ''
          export WORKER_SOCKET="$PWD/pro/worker-socket"
          '';
        };
      };

      apps = {

        test-server = {
          type = "app";
          program = "${buckWorker}";
        };

        test-client = {
          type = "app";
          program = "${test-client.script}";
        };

      };

    });

  nixConfig.extra-substituters = ["https://cache.mercury.com"];
  nixConfig.extra-trusted-public-keys = ["cache.mercury.com:yhfFlgvqtv0cAxzflJ0aZW3mbulx4+5EOZm6k3oML+I="];
  nixConfig.allow-import-from-derivation = true; # needed for cabal2nix
}
