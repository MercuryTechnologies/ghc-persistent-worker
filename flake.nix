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

      mwb-26-07.source.build = {
        url = "https://github.com/MercuryTechnologies/ghc";
        version = "9.10.1";
        flavour = "release+split_sections+ipe";
        # branch: mercury-ghc9101-20260729-with-haddock
        rev = "b88dbe7199178fa0991c489df1e84e1006ae01ee";
        hash = "sha256-xEO44bnUDu+oTsS0Er4dfV1PooTvGOqYYVsGtPsWZe4=";
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
  (import ./ops/tools.nix)
  (import ./ops/package-sets.nix inputs)

  ];

}
