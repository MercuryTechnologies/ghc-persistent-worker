# Run this with `nix run .#comparison-2`
{config, build, util}: let
  inherit (util) lib;

  deps = (import ../ops/test-deps.nix { inherit util; }).deps;

  bin = "${build.packages.dev.buck-worker.package}/bin";

  topdir = "${config.envs.dev.ghc.ghc.ghc}/lib/ghc-9.10.1/lib";

  depArg = {num, package}: let
    ns = builtins.toString num;
  in ["-package" "dep${ns}" "-package-db" "${package}/lib/ghc-9.10.1/lib/package.conf.d"];

  depArgs = lib.concatStringsSep " " (lib.concatMap depArg (lib.take 10 deps));

  commonArgs = "-fwrite-ide-info -no-link -i -dynamic -fPIC -osuf dyn_o -hisuf dyn_hi -package base -fbyte-code-and-object-code --make ${depArgs}";

  timed = "env time -f '%M kB'";

in config.pkgs.writeScript "comparison-2" ''
#!${config.pkgs.runtimeShell}
src="${./.}"

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
${timed} ${bin}/ghc-bin -B${topdir} -odir $out -hidir $out -hiedir $out -dumpdir $out -stubdir $out ${commonArgs} $src/*.hs
clean

echo "------- Start batch worker"
echo ""

${timed} ${bin}/batch-worker '${depArgs}' $src/*.hs

# echo "------- Start Buck worker"
# echo ""
#
# rm /tmp/buck2_worker /tmp/ghc-persistent-worker -rf
# pkill -9 -f 'worker --make'
# nix develop .#buck -c buck clean
# nix develop .#buck -c buck build //comparison-2:comparison-2 -j1 -v 2,stderr
''
