# Prebuilt external dependency packages for the resource test.
#
# Each package is a minimal Haskell library containing a single module that exports an Int value.
# They are compiled by the devshell's GHC and registered in individual package DBs, matching the
# structure expected by Test.ExtDep (one package DB per ext dep).
#
# Packages 0-4 are independent (leaf packages).
# Packages 5-9 depend on the corresponding leaf: extdep5 depends on extdep0, etc.
#
# The result is a directory with the structure:
#   $out/
#     extdep0/
#       package.conf.d/   -- ghc-pkg database for extdep0
#       Extdep0.hi
#       Extdep0.dyn_hi
#       libHSextdep0-ghc<version>.so
#     ...
#     extdep5/
#       package.conf.d/   -- contains only extdep5 (transitive deps provided separately)
#       Extdep5.hi
#       ...
#
# Set resource_test_ext_deps=$out in the devshell to use prebuilt packages instead of compiling at
# test runtime.
{pkgs, lib, ghc}: let

  ghcBin = "${ghc}/bin/ghc";
  ghcPkg = "${ghc}/bin/ghc-pkg";
  ghcVersion = ghc.version;

  extDepName = num: "extdep${toString num}";
  extDepModName = num: "Extdep${toString num}";
  extDepValueName = num: "extdep_value_${toString num}";

  # Build a single external dependency package.
  # `deps` is a list of { num, drv } for packages this one depends on.
  mkExtDep = { num, deps ? [] }: let

    name = extDepName num;
    modName = extDepModName num;
    valueName = extDepValueName num;

    depImports = lib.concatMapStringsSep "\n" (d:
      "import ${extDepModName d.num} (${extDepValueName d.num})"
    ) deps;

    # The module's value sums its own index with all dependency values (to ensure the import is used).
    valueExpr = if deps == []
      then toString (num + 1)
      else lib.concatMapStringsSep " + " (d: extDepValueName d.num) deps + " + ${toString (num + 1)}";

    src = pkgs.writeText "${modName}.hs" ''
      module ${modName} where
      ${depImports}
      ${valueName} :: Int
      ${valueName} = ${valueExpr}
    '';

    # GHC flags to make dependency packages visible during compilation.
    depPkgDbArgs = lib.concatMap (d: ["-package-db" "${d.drv}/${extDepName d.num}/package.conf.d"]) deps;
    depPkgArgs = lib.concatMap (d: ["-package" (extDepName d.num)]) deps;

    depConfEntries = lib.concatMapStringsSep ", " (d: extDepName d.num) deps;

  in pkgs.stdenv.mkDerivation {
    name = "test-${name}";

    dontUnpack = true;

    buildPhase = ''
      mkdir -p pkg
      cp ${src} pkg/${modName}.hs

      # Static compilation (.hi, .o)
      ${ghcBin} -v0 \
        -this-unit-id ${name} \
        -hide-all-packages -package base \
        ${lib.escapeShellArgs depPkgDbArgs} \
        ${lib.escapeShellArgs depPkgArgs} \
        -odir pkg -hidir pkg \
        pkg/${modName}.hs

      # Dynamic compilation (.dyn_hi, .dyn_o)
      ${ghcBin} -v0 \
        -this-unit-id ${name} \
        -hide-all-packages -package base \
        ${lib.escapeShellArgs depPkgDbArgs} \
        ${lib.escapeShellArgs depPkgArgs} \
        -dynamic -osuf dyn_o -hisuf dyn_hi \
        -odir pkg -hidir pkg \
        pkg/${modName}.hs

      # Shared library (.so with GHC version suffix)
      ${ghcBin} -v0 \
        -this-unit-id ${name} \
        -dynamic -shared \
        ${lib.escapeShellArgs depPkgDbArgs} \
        ${lib.escapeShellArgs depPkgArgs} \
        -o pkg/libHS${name}-ghc${ghcVersion}.so \
        pkg/${modName}.dyn_o
    '';

    installPhase = ''
      mkdir -p $out/${name}/package.conf.d

      # Copy artifacts
      cp pkg/${modName}.hi $out/${name}/
      cp pkg/${modName}.dyn_hi $out/${name}/
      cp pkg/libHS${name}-ghc${ghcVersion}.so $out/${name}/

      # Write package config
      cat > $out/${name}/${name}.conf <<EOF
      name: ${name}
      version: 1.0
      id: ${name}
      key: ${name}
      import-dirs: $out/${name}
      library-dirs: $out/${name}
      dynamic-library-dirs: $out/${name}
      hs-libraries: HS${name}
      exposed: True
      exposed-modules: ${modName}
      ${lib.optionalString (deps != []) "depends: ${depConfEntries}"}
      EOF

      # Register only this package in its per-package DB (matching the Buck model where each
      # package has its own DB and the transitive closure is collected separately).
      ${ghcPkg} -v0 --package-db $out/${name}/package.conf.d recache
      ${ghcPkg} -v0 --package-db $out/${name}/package.conf.d register --force $out/${name}/${name}.conf
    '';
  };

  # Leaf packages (0-4): no dependencies.
  leaves = lib.genAttrs (map toString (lib.range 0 4)) (numStr: let
    num = lib.toInt numStr;
  in {
    inherit num;
    drv = mkExtDep { inherit num; };
  });

  # Transitive packages (5-9): extdepN depends on extdep(N-5).
  transitive = map (num: let
    leafNum = num - 5;
    leaf = leaves."${toString leafNum}";
  in mkExtDep {
    inherit num;
    deps = [{ inherit (leaf) num drv; }];
  }) (lib.range 5 9);

  allDeps = (map (entry: entry.drv) (lib.attrValues leaves)) ++ transitive;

in pkgs.symlinkJoin {
  name = "test-ext-deps";
  paths = allDeps;
}
