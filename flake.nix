{
  description = "GHC persistent worker";

  inputs.hix.url = "github:tek/hix/684bfba0b7e70dbba7b6d9232a229787eb356a34";
  inputs.ghc-debug = {
    # url = "git+https://gitlab.haskell.org/ghc/ghc-debug?rev=2541e77d2687b8b3b0c1a52bb4790a602ce17d7d";
    url = "path:/data/mercury/code/haskell/ghc-debug";
    flake = false;
  };
  inputs.fenix = {
    url = "github:nix-community/fenix/9d17341a4f227fe15a0bca44655736b3808e6a03";
    inputs.nixpkgs.follows = "hix/nixpkgs";
  };

  outputs = {hix, ghc-debug, fenix, ...}: let

    testEnv = config: {
      ghc_dir = "${config.ghc.vanillaGhc.ghc}";
      extra_dir = "${config.ghc.vanillaGhc.extra}";
      clock_dir = "${config.ghc.vanillaGhc.clock}";
    };

    envOverrides = config: {overrideAttrs, notest, nodoc, ...}: {
      ghc-persistent-worker-plugin = nodoc notest;
      buck-worker = nodoc (notest (overrideAttrs (testEnv config)));
    };

    ipeOverrides = {ghcOptions, ...}: let
      opts = ghcOptions ["-finfo-table-map" "-fdistinct-constructor-tables"];
    in {
      ghc-persistent-worker-plugin = opts;
      buck-worker = opts;
      debug = opts;
    };

    ghcBuild = {
      enable = true;
      version = "9.10.1";
      url = "https://gitlab.haskell.org/ghc/ghc";
      rev = "6d8c65a7096bba40ba1ba208e8fbfb0349de6ff0";
      sha256 = "sha256-41ngesQck2MlM4NvAtjAdNbF9GOu7zhZJwx0EDeABBY=";
      flavour = "release+split_sections";
    };

    ghcBuildIpe = ghcBuild // { flavour = "release+split_sections+ipe"; };

  in hix ({config, build, lib, util, ...}: let

    testDeps = import ./ops/test-deps.nix { inherit util; };

  in {

    compiler = "ghc910";
    ghcVersions = [];
    main = "buck-worker";
    ghci.args =
      ["-package ghc"]
      ++ ["-DMWB"]
      ;
    hls.genCabal = false;

    envs.dev = args: {
      env = testEnv args.config;
      hls.enable = lib.mkForce false;
      ghc.build = ghcBuildIpe;
      overrides = [
        ({modify, hsLibC, ...}: {
          buck-worker = modify hsLibC.enableSharedExecutables;
        })
        ipeOverrides
        (envOverrides args.config)
      ];
    };

    envs.profiled = args: {
      env = testEnv args.config;
      overrides = [
        ipeOverrides
        (envOverrides args.config)
      ];
      ghc.build = ghcBuildIpe;
    };

    # ------------------------------------------------------------------------------------------------------------------
    # Buck

    # The environment for the CLI tool `buck`, using the Buck overlay extracted from MWB.
    # `fenix` is a dep of Buck.
    envs.buck = {
      expose.shell = true;
      packages = [];
      buildInputs = pkgs: [pkgs.buck2-source];

      ghc.overlays = [
        fenix.overlays.default
        (import ./ops/buck/overlay.nix)
      ];
    };

    # The environment for our Buck nixpkgs integration, from which GHC and the package set are taken when exposing them
    # in `outputs.packages` below.
    # Uses our custom GHC build and injects a hook into all Haskell derivations that creates `package.cache` in the
    # store dir, which is needed because Buck supplies individual package DBs to GHC.
    envs.buck-build = {
      packages = [];
      ghc.build = ghcBuildIpe;

      overrides = api@{override, ...}: testDeps.overrides api // {
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

    outputs.apps.comparison-1 = {
      type = "app";
      program = "${import ./comparison-1/run.nix { inherit config build; }}";
    };

    outputs.apps.comparison-2 = {
      type = "app";
      program = "${import ./comparison-2/run.nix { inherit config build util; }}";
    };

    packages = {

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
            "ghc-debug-stub"
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
          ghc-options = [
            "-O2"
          ];
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
            "-O2"
            "-threaded"
            "-rtsopts"
            ''"-with-rtsopts=-K512M -H -I5 -T -N"''
          ];
          source-dirs = ".";
        };
        executables.batch-worker = {
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
            "-O2"
            "-threaded"
            "-rtsopts"
            ''"-with-rtsopts=-K512M -H -I5 -T -N"''
          ];
        };
        executables.ghc-bin = {
          dependencies = [
            "bytestring"
            "containers"
            "deepseq"
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
          ghc-options-exe = [
            "-O2"
            "-threaded"
            "-rtsopts"
            ''"-with-rtsopts=-K512M -H -I5 -T"''
          ];
        };
        test = {
          enable = true;
          dependencies = [
            "containers"
            "directory"
            "filepath"
            "ghc"
            "ghc-debug-stub"
            "ghc-experimental"
            "ghc-persistent-worker-plugin"
            "temporary"
            "transformers"
            "typed-process"
          ];
          default-extensions = ["OverloadedLists"];
          ghc-options-exe = [
            "-O2"
            "-threaded"
            "-rtsopts"
            ''"-with-rtsopts=-K512M -H -I5 -T -N"''
          ];
          source-dirs = "test";
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

      ghc-persistent-worker-plugin = {
        src = ./plugin;
        library = {
          enable = true;
          dependencies = [
            "bytestring"
            "containers"
            "deepseq"
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
          ghc-options = ["-DMWB" "-O2"];
          component = {
            other-modules = [
              "GHC.Main"
              "Logger"
            ];
          };
        };
      };

    };

    overrides = {hackage, jailbreak, unbreak, force, transformDrv, pkgs, hsLibC, source, ...}: {
      auto-update = hackage "0.2.6" "0sp25j3fcgmfr2zv1ccg1id1iynj3azinjg23g0vy1m1m7gnmkzi";
      eventlog2html = hackage "0.11.1" "0l4klmfsxmikh8x7rp7l3s5sycwq2xmqz3d1p6078pcygjkzc6fv";
      ghc-debug-brick = source.sub ghc-debug "ghc-debug-brick";
      ghc-debug-client = force (source.sub ghc-debug "client");
      ghc-debug-common = force (source.sub ghc-debug "common");
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

  });
}
