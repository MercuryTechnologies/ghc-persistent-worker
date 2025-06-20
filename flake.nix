{
  description = "GHC persistent worker";

  inputs.hix.url = "github:tek/hix";
  inputs.ghc-debug = {
    url = "github:tek/ghc-debug/59198910da4573612bf5c6f9969c64fa620db396";
    flake = false;
  };
  inputs.fenix = {
    url = "github:nix-community/fenix/9d17341a4f227fe15a0bca44655736b3808e6a03";
    inputs.nixpkgs.follows = "hix/nixpkgs";
  };

  outputs = {hix, ghc-debug, fenix, ...}: let

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

  in hix ({config, build, lib, util, ...}: let

    buckTest = dir: {
      expose = true;
      env = "buck";
      command = "${import ./ops/buck-test/${dir} { inherit util; }}";
    };

  in {

    compiler = "ghc910";
    ghcVersions = [];
    main = "ghc-worker";
    ghci.args =
      ["-package ghc"]
      ++ ["-DMWB"]
      ;
    hls.genCabal = false;

    compilers = {

      mwb.source.build = {
        version = "9.10.1";
        url = "https://gitlab.haskell.org/ghc/ghc";
        rev = "2c4d9f6151898e1da7721d39e0ef30bdfb0b9e44";
        hash = "sha256-sTSEiKRRS9+n4M/mI9IbBvmcahGynVksoWlEH/OMzJQ=";
        flavour = "release+split_sections";
      };

      mwb-ipe = {
        extends = "mwb";
        source.build.flavour = "release+split_sections+ipe";
      };

      mwb-hybrid.source.build = {
        version = "9.10.1";
        url = "https://gitlab.haskell.org/ghc/ghc";
        rev = "f9587b0fcc14565953326d0032a3e92a871d0468";
        hash = "sha256-eSFKCb2jgcxdNuy1NaLgkmjkZrr1FD09RbNm+c8FUW4=";
        flavour = "release+split_sections+ipe";
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

    envs.hybrid = args: {
      expose.scoped = true;
      env = testEnv args.config;
      package-set.compiler = "mwb-hybrid";
      overrides = [
        sharedExeOverrides
        ipeOverrides
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

    # ------------------------------------------------------------------------------------------------------------------
    # Buck

    # The environment for the CLI tool `buck`, using the Buck overlay extracted from MWB.
    # `fenix` is a dep of Buck.
    # Exposes a devShell named `buck` that should be used to gain access to the CLI tool.
    envs.buck = {
      expose.shell = true;
      packages = [];
      buildInputs = pkgs: [pkgs.buck2-source];

      package-set.compiler.nixpkgs.overlays = [
        fenix.overlays.default
        (import ./ops/buck/overlay.nix)
      ];
    };

    # The environment for our Buck nixpkgs integration, from which GHC and the package set are taken when exposing them
    # in `outputs.packages` below.
    # Uses our custom GHC build and injects a hook into all Haskell derivations that creates `package.cache` in the
    # store dir, which is needed because Buck supplies individual package DBs to GHC.
    envs.buck-build = {config, ...}: {
      packages = [];
      package-set.compiler = "mwb-ipe";
      env = testEnv config;

      overrides = api@{override, ...}: let
        testDeps = import ./ops/test-deps.nix { inherit util; };
      in testDeps.overrides api // {
        __all = override (drv: {
          postInstall = (drv.postInstall or "") + ''
            ghc-pkg recache --package-db $packageConfDir
          '';
        });
      };
    };

    # The interface that Buck expects when loading Nix packages in `toolchains/BUCK` using those `nix.rules.flake`
    # rules.
    # Exposes the toolchain Haskell packages listed in `./ops/ghc-toolchain-libraries.nix` in the attribute
    # `haskellPackages.libs` as well as Python and the GHC compiler derivation.
    outputs.packages =
      import ./ops/buck/packages.nix { inherit config lib; };

    # ------------------------------------------------------------------------------------------------------------------

    envs.hls-db = {};

    commands.hls.env = "hls-db";

    output.extraPackages = ["ghc-debug-brick" "eventlog2html" "hp2pretty" "ghc-events"];

    commands.comparison-1 = buckTest "comparison1";
    commands.comparison-2 = buckTest "comparison2";
    commands.three-layers = buckTest "three-layers";
    commands.hybrid = buckTest "hybrid";

    packages = {

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

        executables.batch-worker = {};

        executables.ghc-bin = {
          dependencies = [
            "containers"
            "filepath"
            "ghc"
            "ghc-boot"
            "transformers"
          ];
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
            "buck-worker-types"
            "bytestring"
            "containers"
            "deepseq"
            "directory"
            "exceptions"
            "filepath"
            "ghc"
            "text"
          ];
          source-dirs = "src";
          ghc-options = ["-DMWB" "-O2"];
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
          ghc-options = ["-DMWB" "-O2"];
        };
      };

      buck-worker-types = {
        src = ./types;
        library = {
          enable = true;
          dependencies = [
            "containers"
            "filepath"
            "ghc"
            "split"
          ];
          source-dirs = "src";
          ghc-options = ["-DMWB" "-O2"];
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
          ghc-options = ["-DMWB" "-O2"];
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

    overrides = {hackage, jailbreak, unbreak, force, transformDrv, pkgs, hsLibC, source, ...}: {
      auto-update = hackage "0.2.6" "0sp25j3fcgmfr2zv1ccg1id1iynj3azinjg23g0vy1m1m7gnmkzi";
      eventlog2html = hackage "0.11.1" "0l4klmfsxmikh8x7rp7l3s5sycwq2xmqz3d1p6078pcygjkzc6fv";
      ghc-debug-brick = source.sub ghc-debug "ghc-debug-brick";
      ghc-debug-client = force (source.sub ghc-debug "client");
      ghc-debug-common = force (source.sub ghc-debug "common");
      ghc-debug-convention = force (source.sub ghc-debug "convention");
      ghc-debug-stub = source.sub ghc-debug "stub";
      grapesy = jailbreak (hackage "1.0.1" "0j7w0knclrhxc5h1vlbdpwvvpz6ixjw6flqfhdgk6xw30g7cwf5m");
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
