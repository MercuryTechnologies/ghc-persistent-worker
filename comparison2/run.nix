# Run this with `nix run .#comparison-2`
{config, build, util}: let
  inherit (util) lib;

  deps = (import ../ops/test-deps.nix { inherit util; }).deps;

  bin = "${build.packages.dev.ghc-worker.package}/bin";

  topdir = "${config.envs.dev.ghc.ghc.ghc}/lib/ghc-9.10.1/lib";

  depArg = {num, package}: let
    ns = builtins.toString num;
  in ["-package" "dep${ns}" "-package-db" "${package}/lib/ghc-9.10.1/lib/package.conf.d"];

  depArgs = lib.concatStringsSep " " (lib.concatMap depArg (lib.take 100 deps));

  commonArgs = "-fwrite-ide-info -no-link -i -dynamic -fPIC -osuf dyn_o -hisuf dyn_hi -package base -fbyte-code-and-object-code -fprefer-byte-code --make ${depArgs}";

  timed = "env time -f '%E %M kB'";

in config.pkgs.writeScript "comparison-2" ''
#!${config.pkgs.runtimeShell}
set -eu

src="./comparison2/project"

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
${timed} ${bin}/ghc-bin +RTS -t -RTS -B${topdir} -odir $out -hidir $out -hiedir $out -dumpdir $out -stubdir $out ${commonArgs} $src/*.hs
clean

echo "------- Start batch worker"
echo ""

${timed} ${bin}/batch-worker '${depArgs}' $src/*.hs

echo "------- Start Buck worker"
echo ""

nix build --out-link result-ghc-worker .#ghc-worker
nix build --out-link result-buck-proxy .#buck-proxy

rm /tmp/buck2_worker /tmp/ghc-persistent-worker -rf
pkill -9 -f 'worker --make' || true
nix develop .#buck -c buck clean
nix develop .#buck -c buck build //comparison2/project/... -j1 -v 2,stderr
''
