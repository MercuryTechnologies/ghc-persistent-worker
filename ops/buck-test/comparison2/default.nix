# Run this with `nix run .#comparison-2`
{util}: let
  inherit (util) lib build config;

  deps = (import ../../test-deps.nix { inherit util; }).deps;

  bin = "${build.packages.ipe.ghc-worker.package}/bin";

  ghcDir = "${config.envs.ipe.toolchain.packages.ghc}";

  topdir = "${ghcDir}/lib/ghc-9.10.1/lib";

  depArg = {num, package}: let
    ns = builtins.toString num;
  in ["-package" "dep${ns}" "-package-db" "${package}/lib/ghc-9.10.1/lib/package.conf.d"];

  depArgs = lib.concatStringsSep " " (lib.concatMap depArg (lib.take 100 deps));

  commonArgs = "-fwrite-ide-info -no-link -i -dynamic -fPIC -osuf dyn_o -hisuf dyn_hi -package base -fbyte-code-and-object-code -fprefer-byte-code --make ${depArgs}";

  timed = "env time -f '%E %M kB'";

in config.pkgs.writeScript "comparison-2" ''
#!${config.pkgs.runtimeShell}
set -eu

src="${./.}"
export ghc_dir="${ghcDir}"

clean()
{
  rm -f *.{hi,dyn_hi,o,dyn_o,hie} || true >/dev/null
}

tmp=$(mktemp -d)
trap "rm -rf $tmp" EXIT

out="$tmp/out"
mkdir -p $out

echo "------- Start vanilla GHC"
echo ""

clean
${timed} ${bin}/ghc-bin +RTS -t -RTS -B${topdir} -odir $out -hidir $out -hiedir $out -dumpdir $out -stubdir $out ${commonArgs} $src/project/*.hs
clean

echo "------- Start batch worker"
echo ""

${timed} ${bin}/batch-worker '${depArgs}' $src/project/*.hs

echo "------- Start Buck worker"
echo ""

buck kill
buck build //ops/buck-test/comparison2/project/... -j1 -v 2,stderr
buck kill
''
