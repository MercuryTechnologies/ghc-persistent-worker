{config, lib}:
 let
  env = config.envs.buck-build;
  pkgs = env.ghc.pkgs;
  hsPkgs = env.ghc.ghc;

  toolchainLibraries = import ../ghc-toolchain-libraries.nix;

  haskellPackages =
    builtins.listToAttrs (builtins.map (p: { "name" = p.pname; "value" = p; }) haskellLibraries);

  haskellLibraries =
    let
      packages = builtins.map (n: hsPkgs."${n}" or null) toolchainLibraries;
      isHaskellLibrary = p: p ? isHaskellLibrary;
    in
    builtins.filter isHaskellLibrary (lib.closePropagation packages);

in {
  haskellPackages = pkgs.stdenvNoCC.mkDerivation {
    name = "haskellPackages";
    passthru = { libs = haskellPackages; };
    dontBuild = true;
    dontUnpack = true;
    dontConfigure = true;

    installPhase = ''
      mkdir $out
      printf "%s\n" ${ pkgs.lib.strings.concatStringsSep " " (builtins.attrValues haskellPackages) } > $out/packages
    '';
  };
  bash = pkgs.bash-buck;
  python = pkgs.python3;
  ghc = hsPkgs.ghc;
}
