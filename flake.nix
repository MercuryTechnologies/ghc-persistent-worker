{
  description = "GHC persistent worker";

  inputs = {
    hix.url = "git+https://git.tryp.io/tek/hix";
    nixpkgs_old.url = "github:nixos/nixpkgs/nixos-23.11";
    grpc-haskell = {
      url = "github:awakesecurity/grpc-haskell/4b3b04d0ac9d1db53033fb4f0b6864f25b6e945d";
      flake = false;
    };
    proto3-suite = {
      url = "github:tek/proto3-suite/337b03ea7c1b75da577251a06f07d4ad19fdc4ac";
      flake = false;
    };
  };

  outputs = {hix, proto3-suite, grpc-haskell, nixpkgs_old, ...}:
  hix ({config, ...}: let

    pkgs_old = import nixpkgs_old { inherit (config) system; };

    oldGrpc = {
      grpc = pkgs_old.grpc;
      gpr = pkgs_old.grpc;
    };

  in {

    compiler = "ghc910";
    ghcVersions = ["ghc910"];
    main = "buck-worker";
    ghci.args = ["-package ghc"];
    hls.genCabal = false;

    overrides = {cabalOverrides, ghcOptions, source, jailbreak, force, cabal2nixArgs, ...}: {
      range-set-list = jailbreak;
      insert-ordered-containers = jailbreak;
      proto3-suite = force (cabal2nixArgs "-f-large-records -f-swagger" (source.root proto3-suite));
      grpc-haskell-core = cabalOverrides oldGrpc (force (ghcOptions "-Wwarn" (source.sub grpc-haskell "core")));
      grpc-haskell = force (source.root grpc-haskell);
    };

    envs.dev = {config, ...}: let
      env = {
        ghc_dir = "${config.ghc.vanillaGhc.ghc}";
        extra_dir = "${config.ghc.vanillaGhc.extra}";
        clock_dir = "${config.ghc.vanillaGhc.clock}";
      };
    in {
      inherit env;
      buildInputs = [pkgs_old.grpc];
      overrides = {overrideAttrs, ...}: {
        buck-worker = overrideAttrs env;
      };
    };

    internal.hixCli.dev = true;

    packages = {

      ghc-persistent-worker-comm = {
        src = ./comm;
        library = {
          enable = true;
          dependencies = [
            "binary"
            "bytestring"
            "containers"
            "deepseq"
            "filepath"
            "network"
            "time"
            "transformers"
            "unix"
          ];
          source-dirs = "src";
        };
      };

      ghc-persistent-worker-plugin = {
        src = ./plugin;
        library = {
          enable = true;
          dependencies = [
            "bytestring"
            "containers"
            "directory"
            "exceptions"
            "filepath"
            "ghc"
            "ghc-boot"
            "text"
            "time"
            "transformers"
            "unix"
          ];
          source-dirs = ["src" "src-ghc98"];
          component = {
            other-modules = [
              "GHC.Main"
              "Logger"
            ];
          };
        };
      };

      ghc-persistent-worker-client = {
        src = ./client;
        executables.Client = {
          dependencies = [
            "binary"
            "bytestring"
            "ghc-persistent-worker-comm"
            "network"
          ];
          source-dirs = "app";
          component.main = "Client.hs";
        };
      };

      ghc-persistent-worker-server = {
        src = ./server;
        library = {
          enable = true;
          dependencies = [
            "binary"
            "bytestring"
            "containers"
            "directory"
            "extra"
            "filepath"
            "ghc-persistent-worker-comm"
            "network"
            "optparse-applicative"
            "process"
            "stm"
            "transformers"
            "unix"
          ];
        };
        executables.Server = {
          dependencies = [
            "binary"
            "bytestring"
            "containers"
            "directory"
            "extra"
            "filepath"
            "ghc-persistent-worker-comm"
            "ghc-persistent-worker-server"
            "network"
            "optparse-applicative"
            "process"
            "stm"
            "transformers"
            "unix"
          ];
          source-dirs = "app";
        };
      };

      buck-worker = {
        src = ./buck-worker;
        cabal.meta.synopsis = "Buck2 GHC persistent worker";
        library = {
          enable = true;
          dependencies = [
            "bytestring"
            "containers"
            "deepseq"
            "directory"
            "extra"
            "filepath"
            "ghc"
            "ghc-persistent-worker-plugin"
            "grpc-haskell"
            "proto3-suite"
            "proto3-wire"
            "temporary"
            "transformers"
            "text"
            "typed-process"
            "vector"
          ];
          default-extensions = ["OverloadedLists"];
        };
        executables.worker = {
          dependencies = [
            "containers"
            "text"
            "vector"
            "async"
            "directory"
            "filepath"
            "grpc-haskell"
            "ghc"
            "ghc-persistent-worker-plugin"
          ];
          default-extensions = ["OverloadedLists"];
          ghc-options-exe = [
            "-threaded"
            "-rtsopts"
            ''"-with-rtsopts=-K512M -H -I5 -T -N"''
          ];
          source-dirs = "app";
        };
        test.enable = true;
      };

      buck-multiplex-worker = {
        src = ./buck-multiplex-worker;
        cabal.meta.synopsis = "Buck2 GHC persistent worker using the multiplexer daemon";
        executables.multiplex-worker = {
          dependencies = [
            "containers"
            "ghc"
            "ghc-persistent-worker-comm"
            "ghc-persistent-worker-plugin"
            "ghc-persistent-worker-server"
            "grpc-haskell"
            "stm"
            "text"
            "transformers"
            "vector"
            "buck-worker"
          ];
          default-extensions = ["OverloadedLists"];
          source-dirs = ".";
        };
      };

    };

    cabal = {
      author = "Ian-Woo Kim";
      license = "MIT";
      license-file = "LICENSE";
      meta.maintainer = "ianwookim@gmail.com";

      default-extensions = [
        "BlockArguments"
        "DerivingStrategies"
        "DuplicateRecordFields"
        "LambdaCase"
        "NamedFieldPuns"
        "OverloadedRecordDot"
        "OverloadedStrings"
        "RecordWildCards"
        "StrictData"
      ];

      ghc-options = [
        "-Wall"
        "-Widentities"
        "-Wincomplete-uni-patterns"
        "-Wmissing-deriving-strategies"
        "-Wredundant-constraints"
        "-Wunused-type-patterns"
        "-Wunused-packages"
      ];
    };

  });
}
