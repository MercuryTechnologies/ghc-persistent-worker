{
  description = "GHC persistent worker";

  inputs = {
    hix.url = "github:tek/hix";
    hix.inputs.nixpkgs.url = "github:nixos/nixpkgs/b2243f41e860ac85c0b446eadc6930359b294e79";
    ghc-debug = {
      url = "git+https://gitlab.haskell.org/ghc/ghc-debug";
      flake = false;
    };
    fenix = {
      url = "github:nix-community/fenix/6c51b42ac2c25328067956ff980572482786d20c";
      inputs.nixpkgs.url = "github:nixos/nixpkgs/9807714d6944a957c2e036f84b0ff8caf9930bc0";
    };
    nix.url = "github:nixos/nix/2.34.2";
  };

  outputs = inputs@{hix, ...}: hix [({config, lib, util, ...}: {
    ghcVersions = ["ghc914"];
    main = "ghc-worker";
    ghci.args = ["-package ghc"];
    hls.genCabal = false;

    compilers = {

      # Roughly the GHC used by MWB.
      mwb-26-04.source.build = {
        url = "https://gitlab.haskell.org/ghc/ghc";
        version = "9.10.1";
        flavour = "release+split_sections+ipe";
        rev = "cd00672d9eb50cdad53d37499c93b0b022efe21a";
        hash = "sha256-Kzo23TE8Mgk7lesLXkodkbmqa0Mt9rpV7N5MwBMO5eY=";
      };

      # Some as `mwb-26-04`, but with fixed nodes.
      mwb-26-04-fixed.source.build = {
        url = "https://gitlab.haskell.org/ghc/ghc";
        version = "9.10.1";
        flavour = "release+split_sections+ipe";
        rev = "debb34edd90921c93f4c8fe67cbb30615951e843";
        hash = "sha256-OfH9kwzJMRFHS1rGzUZhEYEKnh1PLnc94xBL9MrnS0M=";
      };

      ghc914.nixpkgs = "ghc914";

    };

    nixpkgs = {

      ghc914.source = {
        rev = "c6d65881c5624c9cae5ea6cedef24699b0c0a4c0";
        hash = "sha256-WNGcmeOZ8Tr9dq6ztCspYbzWFswr2mPebM9LpsfGxPk=";
      };

    };

    internal.hixCli.dev = true;

  })

  (import ./ops/packages.nix)
  (import ./ops/package-sets.nix inputs)

  ];

}
