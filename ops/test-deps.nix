{util}: let

  inherit (util) pkgs lib config;

  testDep = num: let

    name = "dep${builtins.toString num}";

    modName = "Dep${builtins.toString num}";

    bind = bnum: let
      bname = "${name}_${builtins.toString bnum}";
    in [
      "${bname} :: ExpQ"
      "${bname} = lift @_ @Int ${builtins.toString bnum}"
    ];

    binds = lib.concatMap bind (lib.range 1 100);

    mod = pkgs.writeText "${modName}.hs" ''
    module ${modName} where
    import Language.Haskell.TH (ExpQ)
    import Language.Haskell.TH.Syntax (lift)
    ${util.unlines binds}
    '';

    cabalName = "${name}.cabal";

    cabal = pkgs.writeText cabalName ''
    cabal-version: 3.0
    name: ${name}
    version: 1
    build-type: Simple
    library
      source-dirs: "."
      exposed-modules: ${modName}
      build-depends: base, template-haskell
      default-extensions:
        TypeApplications
      default-language: GHC2021
    '';

    src = pkgs.stdenv.mkDerivation {
      name = "${name}-src";

      buildCommand = ''
      mkdir $out
      cp ${cabal} $out/${cabalName}
      cp ${mod} $out/${modName}.hs
      '';

    };

  in pkgs.haskell.lib.compose.overrideCabal (drv: {
    doCheck = false;
    doHaddock = false;
    postInstall = (drv.postInstall or "") + ''
    ghc-pkg recache --package-db $packageConfDir
    '';
  }) (config.envs.dev.ghc.ghc.callCabal2nix name src {});

  deps = map (num: { inherit num; package = testDep num; }) (lib.range 1 100);

in {
  inherit deps;
  paths = pkgs.writeText "test-deps" (util.unlines (map ({package, ...}: package) deps));

  overrides = {drv, ...}:
  lib.listToAttrs (map ({num, package}: lib.nameValuePair "dep${builtins.toString num}" (drv package)) deps);

}
