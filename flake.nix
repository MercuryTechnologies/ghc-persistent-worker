{
  description = "GHC persistent worker";

  inputs.hix.url = "github:tek/hix";
  inputs.ghc-debug = {
    url = "github:tek/ghc-debug/59198910da4573612bf5c6f9969c64fa620db396";
    flake = false;
  };

  outputs = {hix, ghc-debug, ...}: let

    testEnv = config: {
      ghc_dir = "${config.toolchain.vanilla.ghc}";
    };

    sharedExeOverrides = {modify, hsLibC, ...}: {
      ghc-worker = modify hsLibC.enableSharedExecutables;
    };

    envOverrides = config: {overrideAttrs, notest, nodoc, ...}: {
      ghc-worker = nodoc (overrideAttrs (testEnv config));
      buck-worker-internal = nodoc;
      buck-worker-proto = nodoc;
      buck-worker-types = nodoc;
    };

    ipeOverrides = {ghcOptions, ...}: let
      opts = ghcOptions ["-finfo-table-map" "-fdistinct-constructor-tables"];
    in {
      ghc-worker = opts;
      buck-worker-internal = opts;
      buck-worker-proto = opts;
      buck-worker-types = opts;
      debug = opts;
    };

    buckBinOverrides = {overrideAttrs, notest, nodoc, ...}: {
      ghc-worker = notest;
    };

  in hix ({config, build, lib, ...}: {

    compiler = "ghc910";
    ghcVersions = [];
    main = "ghc-worker";
    ghci.args =
      ["-package ghc"]
      ++ ["-DMWB" "-DMWB_2025_07"]
      ;
    hls.genCabal = false;

    compilers = {

      mwb.source.build = {
        version = "9.10.1";
        url = "https://gitlab.haskell.org/ghc/ghc";
        rev = "0d3a068a982c8f95c321e53ddf9c1b8b482264d8";
        hash = "sha256-X8cw0J2Vrh7AY27QpAkJJx3GY4csi4f6UBYTx4QCbO4=";
        flavour = "release+split_sections";
      };

      mwb-ipe = {
        extends = "mwb";
        source.build.flavour = "release+split_sections+ipe";
      };

    };

    envs.dev = args: {
      env = testEnv args.config;
      hls.enable = lib.mkForce false;
      package-set.compiler = "mwb-ipe";
      overrides = [
        sharedExeOverrides
        ipeOverrides
        (envOverrides args.config)
      ];
    };

    envs.ipe = args: {
      expose.scoped = true;
      env = testEnv args.config;
      package-set.compiler = "mwb-ipe";
      overrides = [
        sharedExeOverrides
        ipeOverrides
        (envOverrides args.config)
        buckBinOverrides
      ];
    };

    envs.no-ipe = args: {
      expose.scoped = true;
      env = testEnv args.config;
      package-set.compiler = "mwb";
      overrides = [
        sharedExeOverrides
        (envOverrides args.config)
        buckBinOverrides
      ];
    };

    envs.profiled = args: {
      env = testEnv args.config;
      overrides = [
        ipeOverrides
        (envOverrides args.config)
      ];
      package-set.compiler = "mwb-ipe";
    };

    envs.hls-db = {};

    commands.hls.env = "hls-db";

    output.extraPackages = ["ghc-debug-brick" "eventlog2html" "hp2pretty" "ghc-events"];

    packages = let
      ghc-options = ["-DMWB" "-DMWB_2025_07" "-O2"];
    in {

      ghc-worker = {
        src = ./ghc-worker;
        cabal.meta.synopsis = "Buck2 GHC persistent worker";
        cabal.ghc-options-exe = [
          "-O2"
          "-threaded"
          "-rtsopts"
          ''"-with-rtsopts=-K512M -H -I5 -T -N"''
        ];

        library = {
          enable = true;
          dependencies = [
            "async"
            "buck-worker-grpc"
            "buck-worker-internal"
            "buck-worker-proto"
            "buck-worker-types"
            "containers"
            "deepseq"
            "directory"
            "exceptions"
            "filepath"
            "ghc"
            "ghc-debug-stub"
            "grapesy"
            "process"
            "stm"
            "temporary"
            "text"
            "unix"
          ];
        };

        executables.ghc-worker = {
          source-dirs = "app/ghc-worker";
        };

        test = {
          enable = true;
          dependencies = [
            "buck-worker-internal"
            "buck-worker-types"
            "containers"
            "directory"
            "filepath"
            "ghc"
            "temporary"
            "typed-process"
          ];
          source-dirs = "test";
          dependOnLibrary = false;
        };
      };

      debug = {
        src = ./debug;
        cabal.dependencies = ["ghc-debug-client" "ghc-debug-common" "ghc-debug-stub" "containers"];
        executable.enable = true;
        executables.snapshot = {
          dependencies = ["directory" "filepath"];
        };
        executables.gen-case = {};
      };

      buck-proxy = {
        src = ./buck-proxy;
        cabal.meta.synopsis = "Buck2 GHC persistent worker";
        library = {
          enable = true;
          dependencies = [
            "buck-worker-grpc"
            "buck-worker-proto"
            "buck-worker-types"
            "containers"
            "deepseq"
            "directory"
            "grapesy"
            "process"
            "text"
          ];
        };
        executables.buck-proxy = {
          dependencies = [
            "buck-worker-types"
            "unix"
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
            "aeson"
            "buck-worker-types"
            "bytestring"
            "containers"
            "deepseq"
            "directory"
            "exceptions"
            "filepath"
            "ghc"
            "text"
            "transformers"
          ];
          source-dirs = "src";
          inherit ghc-options;
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
          inherit ghc-options;
        };
      };

      buck-worker-types = {
        src = ./types;
        library = {
          enable = true;
          dependencies = [
            "aeson"
            "bytestring"
            "containers"
            "filepath"
            "ghc"
            "split"
            "text"
          ];
          source-dirs = "src";
          inherit ghc-options;
        };
      };

      buck-worker-grpc = {
        src = ./grpc;
        library = {
          enable = true;
          dependencies = [
            "buck-worker-types"
            "buck-worker-proto"
            "containers"
            "grapesy"
            "text"
          ];
          source-dirs = "src";
          inherit ghc-options;
        };
      };

    };

    # Create two result links for the worker binaries that can be used in MWB with something like:
    # impure_worker(
    #    name = "impure_ghc_worker",
    #    binary_path = "/path/to/persistent-worker/result-no-ipe-ghc-worker/bin/ghc-worker",
    # )
    # The env `no-ipe` is used because unlike the test projects here, MWB doesn't use the custom GHC defined here, so
    # building the worker with IPE will cause some problems.
    outputs.apps.build-mwb-links = util.app (util.zscript "build-mwb-links" ''
    nix build --out-link result-no-ipe-buck-proxy .#no-ipe.buck-proxy
    nix build --out-link result-no-ipe-ghc-worker .#no-ipe.ghc-worker
    '');

    outputs.apps.build-mwb-links-ipe = util.app (util.zscript "build-mwb-links-ipe" ''
    nix build --out-link result-ipe-buck-proxy .#ipe.buck-proxy
    nix build --out-link result-ipe-ghc-worker .#ipe.ghc-worker
    '');

    overrides = {hackage, jailbreak, unbreak, force, transformDrv, pkgs, hsLibC, source, ...}: {
      auto-update = hackage "0.2.6" "0sp25j3fcgmfr2zv1ccg1id1iynj3azinjg23g0vy1m1m7gnmkzi";
      eventlog2html = hackage "0.11.1" "0l4klmfsxmikh8x7rp7l3s5sycwq2xmqz3d1p6078pcygjkzc6fv";
      ghc-debug-brick = source.sub ghc-debug "ghc-debug-brick";
      ghc-debug-client = force (source.sub ghc-debug "client");
      ghc-debug-common = force (source.sub ghc-debug "common");
      ghc-debug-convention = force (source.sub ghc-debug "convention");
      ghc-debug-stub = source.sub ghc-debug "stub";
      grapesy = force (hackage "1.0.1" "0j7w0knclrhxc5h1vlbdpwvvpz6ixjw6flqfhdgk6xw30g7cwf5m");
      grpc-spec = hackage "1.0.0" "0pgq63k6p65c5ffzxwihp8j1p731qrnda5rxrzqsylanmdmnvjb8";
      http-semantics = hackage "0.3.0" "0ghj37jr5bsz047p6i66ddkwc9mxkfpbw14nd54slmj1lpwn5z4a";
      http2 = hackage "5.3.9" "19lzz6y1rzsbyfswfp9zg9gszmimvfib72dg60x6f19lvmnziga8";
      http2-tls = unbreak;
      network = hackage "3.2.7.0" "08frm9gm422b9aqlmmzflj0yr80ic0ip8s4gsmr0izhizzab5420";
      network-control = hackage "0.1.6" "1d2fa3jbxsajv0najlsi61gdrx43fbf2a08s7iv13q0bii65dy18";
      network-run = hackage "0.4.4" "0c2wpm9bkizaw9sbhy9yi51m04cjlbvzdjw09s5gy74wz2pz4spw";
      proto-lens = force;
      proto-lens-protobuf-types = force;
      proto-lens-protoc = hackage "0.9.0.0" "18b0hz5z4cfimnbhjnhdk4lf2r0wy5aardngdhyy8aqvr62v5r62";
      proto-lens-runtime = force;
      proto-lens-setup = force;
      serialise = force;
      snappy-c = force;
      tls = hackage "2.1.6" "11rxsmwhv6g4298a0355v6flz4n6gw64qw3iha7z0ka3nv7vq4vv";
      time-manager = hackage "0.2.2" "1ja8pimvy07b05ifkrg6q0lzs3kh0k2dmncwjdxl81199r559vf5";
    };

    internal.hixCli.dev = true;

    cabal = {
      author = "Ian-Woo Kim";
      license = "MIT";
      license-file = "LICENSE";
      meta.maintainer = "ianwookim@gmail.com";
      language = "GHC2021";

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

  });
}
