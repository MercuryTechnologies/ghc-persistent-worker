{ghc-debug, ...}:
{config, lib, build, ...}: let

  sharedExeOverrides = {modify, hsLibC, ...}: {
    ghc-proxy = modify hsLibC.enableSharedExecutables;
    ghc-worker = modify hsLibC.enableSharedExecutables;
  };

  allPackages = value: {
    ghc-proxy = value;
    ghc-server = value;
    ghc-worker = value;
    ghc-worker-test-common = value;
    buck-proxy = value;
    buck-worker-grpc = value;
    buck-worker-internal = value;
    buck-worker-proto = value;
    buck-worker-types = value;
    debug = value;
  };

  envOverrides = {overrideAttrs, nodoc, ...}: allPackages nodoc;

  ipeOverrides = {ghcOptions, ...}: let
    opts = ghcOptions ["-finfo-table-map" "-fdistinct-constructor-tables"];
  in allPackages opts;

  buckBinOverrides = {overrideAttrs, notest, nodoc, ...}: {
    buck-worker-internal = notest;
    ghc-worker = notest;
  };

  overrides2607 = {hackage, force, source, self, ...}: {
    auto-update = hackage "0.2.6" "0sp25j3fcgmfr2zv1ccg1id1iynj3azinjg23g0vy1m1m7gnmkzi";
    crypton-asn1-encoding = hackage "0.10.0" "0h4cxk9yz2xgmx0kl3gg9lixhnhvxqk85gvkwldp0mlfm3mgccvm";
    crypton-asn1-parse = hackage "0.10.0" "0dsyslbb9a3f6wj0na52qc7iimjs9xljhi6wjfch61nb9m33l1kb";
    crypton-asn1-types = hackage "0.4.1" "01zvf9vn5a0jyaq5l6mmzv7ya35sxjrk10k06rmi31x128sfqs7s";
    crypton-pem = hackage "0.3.0" "1bvcl2brlgqbb1kmjzlfspmm47n1g441qgsmyhz9ql3zlcz1s524";
    ech-config = hackage "0.0.1" "0sxxxd9rlc3x14mgh92ic8s9hjncf38f9s7p3ic284mvnzj0l3s2";
    eventlog2html = hackage "0.11.1" "0l4klmfsxmikh8x7rp7l3s5sycwq2xmqz3d1p6078pcygjkzc6fv";
    flatparse = hackage "0.5.2.0" "06qncwbrwxpx877xxxq01zap3l33ln7ab5v3jr70mb2h5j6v97ck";
    grapesy = force (hackage "1.1.1" "01n14bcrwshm2vkgrzg10s6rvxsw9sm75ws26py0k3f03bj94jcv");
    grpc-spec = hackage "1.0.0" "0pgq63k6p65c5ffzxwihp8j1p731qrnda5rxrzqsylanmdmnvjb8";
    hedgehog = force (hackage "1.7" "04cjnz4i1qs3v9bza8a3ry1czapwqgxazhywkjzq2rg1544gjmby");
    hinotify = hackage "0.4.2" "072i8d9khxwra5x05bxxm6018ga3sjf7kykxqc6km7vi01wh2h1b";
    hpke = hackage "0.0.0" "0vyny5gqw8rk0s75088ggs3q78fgmas9mnxnwjpny4h9nw6dysr9";
    http-semantics = hackage "0.3.0" "0ghj37jr5bsz047p6i66ddkwc9mxkfpbw14nd54slmj1lpwn5z4a";
    network = hackage "3.2.7.0" "08frm9gm422b9aqlmmzflj0yr80ic0ip8s4gsmr0izhizzab5420";
    network-control = hackage "0.1.7" "0p46ymb8565909q2qzig02q91ch8c4zrkminvma1iizb3s2d81m8";
    network-run = hackage "0.4.4" "0c2wpm9bkizaw9sbhy9yi51m04cjlbvzdjw09s5gy74wz2pz4spw";
    proto-lens = force;
    proto-lens-protobuf-types = force;
    proto-lens-protoc = force (hackage "0.9.0.0" "18b0hz5z4cfimnbhjnhdk4lf2r0wy5aardngdhyy8aqvr62v5r62");
    proto-lens-runtime = force;
    proto-lens-setup = force;
    ram = hackage "0.22.0" "1mwg8gha1y2hvk7yf2kd9411ibqba0r9ach1ypg6yk5mxqrfgcv7";
    serialise = force;
    snappy-c = force;
    tasty = hackage "1.5.4" "0x6khif6n0rzfgkvrbiagg1sj0lwmjfr6qarjnjwmb9ywdk7598b";
    tasty-hedgehog = force;
    time-hourglass = hackage "0.3.0" "11fm4wywl0q5g0q34d049x7wxlp80rycp7hqrp2m7l7dmhihnn6d";
    time-manager = hackage "0.2.2" "1ja8pimvy07b05ifkrg6q0lzs3kh0k2dmncwjdxl81199r559vf5";
    uuid = force;
    zlib = self.zlib_0_7_1_0;
    };

  overrides_mwb_flag = extra: {enable, ...}: let

    flags = builtins.foldl' (z: flag: enable flag z) [] extra;

  in {
    buck-worker-types = flags;
    buck-worker-internal = flags;
    ghc-worker = flags;
    ghc-server = flags;
  };

  commonOverrides = flags: [
    sharedExeOverrides
    envOverrides
    (overrides_mwb_flag flags)
  ];

  defaultEnv = extra: {
    hls.enable = lib.mkForce false;
    package-set.extends = "mwb-26-07";
    overrides = commonOverrides ["mwb" "unit-index" "downsweep-cache"] ++ [ipeOverrides] ++ extra;
    ghci.args = ["-DMWB" "-DDOWNSWEEP_CACHE" "-DUNIT_INDEX"];
  };

  mkGithub = {force, source, nodoc, ...}: {owner ? "tek", repo, rev, hash, path ? ""}:
    nodoc (force (source.sub (config.pkgs.fetchFromGitHub { inherit owner repo rev hash; }) path));

in {

  envs.ghc910 = {
    expose.scoped = true;
    overrides = [envOverrides ({notest, ...}: { ghc-worker = notest; })];
  };

  envs.dev = defaultEnv [] // {
    package-set.extends = "mwb-26-07";
    buildInputs = pkgs: [pkgs.zlib pkgs.snappy pkgs.protobuf build.envs.dev.toolchain.packages.proto-lens-protoc];
    overrides = commonOverrides ["mwb" "unit-index" "downsweep-cache" "fixed-nodes"] ++ [ipeOverrides];
    ghci.args = ["-DMWB" "-DDOWNSWEEP_CACHE" "-DUNIT_INDEX" "-DFIXED_NODES"];
  };

  envs.min = defaultEnv [];

  envs.mwb-26-07-linkables = defaultEnv [] // {
    expose.scoped = true;
    package-set.extends = "mwb-26-07-linkables";
    overrides = commonOverrides ["mwb" "unit-index" "downsweep-cache" "fixed-nodes"] ++ [buckBinOverrides ipeOverrides];
    ghci.args = ["-DMWB" "-DDOWNSWEEP_CACHE" "-DUNIT_INDEX" "-DFIXED_NODES"];
  };

  envs.mwb-26-07 = defaultEnv [] // {
    expose.scoped = true;
    package-set.extends = "mwb-26-07";
  };

  envs.profiled = defaultEnv [({notest, ...}: { ghc-worker = notest; ghc-server = notest; })];

  envs.ghc914 = {
    expose.scoped = true;
    package-set.extends = "ghc914";
    overrides = commonOverrides [] ++ [buckBinOverrides];
  };

  # This environment is for building the worker with an externally provided, impure GHC.
  envs.cabal-build = {
    expose.shell = true;
    package-set.compiler.source = "ghc910";
    package-set.overrides = lib.mkForce [];
    package-set.extraOverrides = lib.mkForce [];
    packages = [];
    buildInputs = pkgs: [pkgs.zlib pkgs.snappy pkgs.protobuf];
  };

  envs.test-ext-deps = let

    testExtDeps = import ./test-ext-deps.nix {
      inherit (config) pkgs;
      inherit lib;
      ghc = build.envs.dev.toolchain.packages.ghc;
    };

  in defaultEnv [] // {
    expose.shell = true;
    env.resource_test_ext_deps = "${testExtDeps}";
    buildInputs = pkgs: [pkgs.zlib];
  };

  # Use GHC 9.8 for `cabal-install` and other build tools because:
  # - If we used the same GHC as the build (i.e. MWB branch), any time the GHC changes, Cabal would be rebuilt, which
  #   is time-consuming.
  # - If we used GHC 9.10 (matching the MWB version), the build GHC's libraries would be shadowed by those used to
  #   build Cabal, because the nixpkgs GHC derivation doesn't set the proper hash suffix.
  #   This is fixed in nixpkgs upstream.
  envs.hix-build-tools.package-set.compiler.source = "ghc98";

  overrides = {hackage, force, source, notest, super, ...}: {
    crypton = hackage "1.0.6" "0y5r1kzlgnzriydik334f5y5bxfm9mf0akxvxs810896r4hnvq0h";
    crypton-x509 = hackage "1.8.0" "0f35689cbxdv25b0xjlla4hmxjxjraiwc6v89y12nl3nxqx3q5f3";
    crypton-x509-store = hackage "1.8.0" "1irrrgm6jmw0irjgwk877smg381wlv72rcgacqrp09dplzjcg82k";
    crypton-x509-validation = hackage "1.8.0" "1dxvbkxwlk6qhg0id65fwssda04pn9y7glq7jpakqlww8d6nl90b";
    ghc-debug-brick = source.sub ghc-debug "ghc-debug-brick";
    ghc-debug-client = force (source.sub ghc-debug "client");
    ghc-debug-common = force (source.sub ghc-debug "common");
    ghc-debug-convention = force (source.sub ghc-debug "convention");
    ghc-debug-stub = source.sub ghc-debug "stub";
    grpc-spec = force;
    http2-tls = force (hackage "0.4.9" "06sw9z3qbsw70phh0fngpa3drg8sdrxiszjlf2i7wxyl04l3n6i4");
    tls = hackage "2.2.2" "1arnw38a3x70264sags3yrq4c01nfcy17sjq3ycasfb2yq6fiflm";
  };

  package-sets.mwb-26-07-linkables = {
    extends = "mwb-26-07";
    compiler = "mwb-26-07-linkables";
    overrides = api@{hackage, force, source, notest, nodoc, nobench, ...}: let

      github = mkGithub api;
    in overrides2607 api // {
      doctest = github {
        repo = "doctest";
        rev = "f6f0ea80314ae97a550229c95b15333566c35fe0";
        hash = "sha256-R3HKHj6+btPodhOyeW50xvZwFqF1IaN3+6dHN9KLjmw=";
      };
    };
  };

  package-sets.mwb-26-07 = {
    compiler = "mwb-26-07";
    overrides = api@{hackage, force, notest, ...}: let
      github = mkGithub api;
    in overrides2607 api // {
      doctest = github {
        repo = "doctest";
        owner = "wavewave";
        # branch: wavewave/0_22_6_fixed_nodes
        rev = "b2bc53a1ebbb2fa48ca1c6b49cfaad8eea8beabc";
        hash = "sha256-qdhfA+AkaB/IZsmeQOfsfZyuPxnY8bbYwO/yHcmjzak=";
      };
    };
  };

  package-sets.ghc914 = {
    compiler.extends = "ghc914";
    overrides = api@{hackage, force, notest, ...}: let

      github = mkGithub api;

    in {
      base64 = force;
      bitwise = force;
      brick = force;
      config-ini = force;
      lens-family = force;
      lens-family-core = force;
      proto-lens = force;
      proto-lens-protobuf-types = force;
      proto-lens-protoc = force;
      proto-lens-runtime = force;
      string-interpolate = force;
      generic-lens = notest;

      proto-lens-setup = github {
        repo = "proto-lens";
        rev = "901331d19c3ab90ec24e231fa69c9ed81204f73b";
        path = "proto-lens-setup";
        hash = "sha256-st+j4vK4N00xHB//b62/HPLRBUw/PRGL8bP8WECMU5U=";
      };

    };
  };

  envs.tools = {
    expose.scoped = true;
    packages = ["ghc-debug-brick" "eventlog2html" "hp2pretty" "ghc-events" "profiteur"];
    overrides = {force, ...}: {
      ghc-prof = force;
      profiteur = force;
    };
  };

  envs.hls-db = {
    package-set.extends = "mwb-26-07";
  };

  commands.hls.env = "hls-db";

  envs.hls = {
    package-set.extends = "mwb-26-07";

    overrides = api@{hackage, fast, force, unbreak, nobench, notest, source, modify, hsLibC, disable, drv, ghcOption, self, ...}: let

      github = path: fast (mkGithub api path);

      rev = "d45b5400b43ec2130ce3197a322891993cb3d73f";
      hash = "sha256-BHBvrVImJAOpm4XY/XeS6Hd2ZzZOcPZuqKRpPKVHGtI=";

      hlsPackage = path:
      ghcOption "-DMWB" (github {
        repo = "haskell-language-server";
        inherit rev hash;
        inherit path;
      });

    in {

      binary-instances = force;

      cabal-add = fast (force (hackage "0.2" "0yxh19iqspai0003p83rsnqkhq2dxa3a2vz3qfzg3k4392z1zbvi"));

      haskell-language-server =
        lib.foldl (lib.flip disable) (modify hsLibC.enableSharedExecutables (hlsPackage "")) [
          "stan" "stylishHaskell" "ormolu" "fourmolu" "hlint"
        ];
      ghcide = hlsPackage "ghcide";
      hls-graph = hlsPackage "hls-graph";
      hls-plugin-api = hlsPackage "hls-plugin-api";
      hls-test-utils = hlsPackage "hls-test-utils";

      hie-bios = github {
        repo = "hie-bios";
        rev = "6847c318cb8524f1d46d2bf02b991318253cef9b";
        hash = "sha256-rR8b2g6Req5Ssr4TtfMCNQZFqRrgG0S+pMj06KkE+q4=";
      };

      Diff = hackage "0.5" "13n231179wa9xm2933f328v00jb486w740yahz4qcbza4yv39w1i";
      co-log-core = notest;
      directory-ospath-streaming = hackage "0.3" "0m0v200mgmkizm3l6pw9x9gvqx9xancgsal4z1pb7hi2pgrj0w0d";
      doctest = github {
        repo = "doctest";
        owner = "wavewave";
        # branch: wavewave/0_22_6_fixed_nodes
        rev = "b2bc53a1ebbb2fa48ca1c6b49cfaad8eea8beabc";
        hash = "sha256-qdhfA+AkaB/IZsmeQOfsfZyuPxnY8bbYwO/yHcmjzak=";
      };
      doctest-parallel = self.doctest;
      fourmolu = drv null;
      ghc-lib-parser = hackage "9.12.2.20250421" "0qxi41zr50chrr6isyfpff5kq6kqxhc5iri6a8ixvz27042a0hsq";
      ghc-lib-parser-ex = hackage "9.12.0.0" "1kxdwr1vpjn4rlhbvajdh25zjl3wyl8lli0krmdxlp03jg4p2vlx";
      hiedb = notest (hackage "0.7.0.0" "0i6szmajpg1w2mi29vs2z3brjhznivaq2his6zcz38gpyfr2dlwi");
      hlint = drv null;
      ormolu = drv null;
      stan = drv null;
      stylish-haskell = drv null;

    };

  };

}
