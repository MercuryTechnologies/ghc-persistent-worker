{
  description = "GHC persistent worker";

  inputs.hix.url = "github:tek/hix";
  inputs.hix.inputs.nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";

  outputs = {hix, ...}: hix {

    compiler = "ghc910";
    ghcVersions = ["ghc910"];
    main = "buck-worker";
    ghci.args = ["-package ghc"];
    hls.genCabal = false;

    envs.dev = {config, ...}: let
      env = {
        ghc_dir = "${config.ghc.vanillaGhc.ghc}";
        extra_dir = "${config.ghc.vanillaGhc.extra}";
        clock_dir = "${config.ghc.vanillaGhc.clock}";
      };
    in {
      inherit env;
      overrides = {overrideAttrs, notest, ...}: {
        buck-worker = notest (overrideAttrs env);
      };
    };

    packages = {

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

      buck-worker = {
        src = ./buck-worker;
        cabal.meta.synopsis = "Buck2 GHC persistent worker";
        library = {
          enable = true;
          dependencies = [
            "async"
            "bytestring"
            "containers"
            "deepseq"
            "directory"
            "exceptions"
            "filepath"
            "ghc"
            "ghc-persistent-worker-plugin"
            "hashable"
            "grapesy"
            "lens-family"
            "process"
            "proto-lens"
            "temporary"
            "text"
            "typed-process"
            "vector"
          ];
          default-extensions = ["OverloadedLists"];
        };
        executables.buck-worker = {
          dependencies = [
            "containers"
            "text"
            "vector"
            "async"
            "directory"
            "filepath"
            "ghc"
            "ghc-persistent-worker-plugin"
            "grapesy"
          ];
          default-extensions = ["OverloadedLists"];
          ghc-options-exe = [
            "-threaded"
            "-rtsopts"
            ''"-with-rtsopts=-K512M -H -I5 -T -N"''
          ];
          source-dirs = ".";
        };
        test = {
          enable = true;
          dependencies = [
            "containers"
            "directory"
            "filepath"
            "ghc"
            "ghc-persistent-worker-plugin"
            "temporary"
            "transformers"
            "typed-process"
          ];
        };
      };

      buck-multiplex-worker = {
        src = ./buck-multiplex-worker;
        cabal.meta.synopsis = "Buck2 GHC persistent worker using the multiplexer daemon";
        executables.multiplex-worker = {
          dependencies = [
            "containers"
            "ghc"
            "ghc-persistent-worker-plugin"
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

    overrides = {hackage, jailbreak, unbreak, force, transformDrv, pkgs, hsLibC, ...}: {
      grapesy = jailbreak (hackage "1.0.1" "0j7w0knclrhxc5h1vlbdpwvvpz6ixjw6flqfhdgk6xw30g7cwf5m");
      grpc-spec = hackage "1.0.0" "0pgq63k6p65c5ffzxwihp8j1p731qrnda5rxrzqsylanmdmnvjb8";
      http2-tls = unbreak;
      network = hackage "3.2.7.0" "08frm9gm422b9aqlmmzflj0yr80ic0ip8s4gsmr0izhizzab5420";
      network-run = hackage "0.4.4" "0c2wpm9bkizaw9sbhy9yi51m04cjlbvzdjw09s5gy74wz2pz4spw";
      proto-lens = force;
      proto-lens-protobuf-types = force;
      proto-lens-protoc = hackage "0.9.0.0" "18b0hz5z4cfimnbhjnhdk4lf2r0wy5aardngdhyy8aqvr62v5r62";
      proto-lens-runtime = force;
      proto-lens-setup = force;
      serialise = force;
      snappy-c = force;
      tls = hackage "2.1.6" "11rxsmwhv6g4298a0355v6flz4n6gw64qw3iha7z0ka3nv7vq4vv";
    };

    internal.hixCli.dev = true;

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
        "TypeFamilies"
        "DataKinds"
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

  };
}
