{
  description = "GHC persistent worker";

  inputs.hix.url = "github:tek/hix";
  inputs.hix.inputs.nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";

  outputs = {hix, ...}: hix {

    compiler = "ghc910";
    ghcVersions = ["ghc910"];
    main = "ghc-worker";
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
        ghc-worker = notest (overrideAttrs env);
      };
    };

    packages = {

      ghc-worker = {
        src = ./ghc-worker;
        cabal.meta.synopsis = "Buck2 GHC persistent worker";
        library = {
          enable = true;
          dependencies = [
            "async"
            "buck-worker-internal"
            "buck-worker-proto"
            "buck-worker-types"
            "bytestring"
            "containers"
            "deepseq"
            "directory"
            "exceptions"
            "filepath"
            "ghc"
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
        };
        executables.ghc-worker = {
          dependencies = [
            "buck-worker-internal"
            "containers"
            "text"
            "vector"
            "async"
            "directory"
            "filepath"
            "ghc"
            "grapesy"
          ];
          ghc-options-exe = [
            "-threaded"
            "-rtsopts"
            ''"-with-rtsopts=-K512M -H -I5 -T -N"''
          ];
          source-dirs = "app/ghc-worker";
        };
        test = {
          enable = true;
          dependencies = [
            "buck-worker-internal"
            "containers"
            "directory"
            "filepath"
            "ghc"
            "temporary"
            "transformers"
            "typed-process"
          ];
          ghc-options-exe = [
            "-O2"
            "-threaded"
            "-rtsopts"
            ''"-with-rtsopts=-K512M -H -I5 -T -N"''
          ];
          source-dirs = "test";
        };
      };

      buck-proxy = {
        src = ./buck-proxy;
        cabal.meta.synopsis = "Buck2 GHC persistent worker";
        library = {
          enable = true;
          dependencies = [
            "buck-worker-proto"
            "buck-worker-types"
            "deepseq"
            "directory"
            "grapesy"
            "process"
          ];
        };
        executables.buck-proxy = {
          dependencies = [
            "buck-worker-types"
          ];
          ghc-options-exe = [
            "-O2"
            "-threaded"
            "-rtsopts"
            ''"-with-rtsopts=-K512M -H -I5 -T -N"''
          ];
          source-dirs = "app/buck-proxy";
        };
      };

      buck-worker-internal = {
        src = ./internal;
        library = {
          enable = true;
          dependencies = [
            "binary"
            "buck-worker-types"
            "bytestring"
            "containers"
            "directory"
            "exceptions"
            "filepath"
            "ghc"
            "ghc-boot"
            "stm"
            "text"
            "time"
            "transformers"
            "unix"
            "vector"
          ];
          source-dirs = ["src" "src-ghc98"];
          ghc-options = ["-O2"];
        };
      };

      buck-worker-proto = {
        src = ./proto;
        library = {
          enable = true;
          dependencies = [
            "bytestring"
            "containers"
            "deepseq"
            "grapesy"
            "lens-family"
            "proto-lens"
            "text"
            "vector"
          ];
          source-dirs = "src";
          ghc-options = ["-O2"];
        };
      };

      buck-worker-types = {
        src = ./types;
        library = {
          enable = true;
          dependencies = [
            "containers"
            "filepath"
            "split"
          ];
          source-dirs = "src";
          ghc-options = ["-O2"];
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
        "OverloadedLists"
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
