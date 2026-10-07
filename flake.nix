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
    ghcVersions = [];
    main = "ghc-worker";
    ghci.args = ["-package ghc"];
    hls.genCabal = false;

    compilers = {

      mercury-ghc9101.source.build = {
        url = "https://github.com/MercuryTechnologies/ghc";
        version = "9.10.1";
        flavour = "release+split_sections+ipe";
        # branch: wavewave/20260912-new
        rev = "da7b82a0a3ce414b1d9efa94dad518a2e2c50dac";
        hash = "sha256-8LPVP7jru1mwb28KLOb7XFb5Bmr/GQSqtNScbq2sKFY=";
      };

      mercury-ghc9141 = {
        nixpkgs = "ghc9141";
        source.build = {
          url = "https://github.com/MercuryTechnologies/ghc";
          version = "9.14.1";
          flavour = "release+split_sections+ipe";
          # branch: wavewave/20260930-unit-index-cache
          rev = "87d8b8f7016021f96fe6f6ea90d7d55543bb4b41";
          hash = "sha256-z7VUe3swBmbU6uggjWHTnZ73YqLj6LEDN587qvOJO/g=";
          bootCompiler = "ghc9103";
        };
      };
    };

    nixpkgs = {

      ghc9141.source = {
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
