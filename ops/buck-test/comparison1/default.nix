# Run this with `nix run .#comparison-1`
{util}: let
  inherit (util) config build;

  bin = "${build.packages.ipe.ghc-worker.package}/bin";

  ghcDir = "${config.envs.ipe.toolchain.packages.ghc}";

  topdir = "${ghcDir}/lib/ghc-9.10.1/lib";

  commonArgs = "-fwrite-ide-info -no-link -i -dynamic -fPIC -osuf dyn_o -hisuf dyn_hi -package base -fbyte-code-and-object-code -fprefer-byte-code --make";

  timed = "env time -f '%M kB'";

in config.pkgs.writeScript "comparison-1" ''
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
${timed} ${bin}/ghc-bin -B${topdir} -odir $out -hidir $out -hiedir $out -dumpdir $out -stubdir $out ${commonArgs} $src/project/*.hs
clean

echo "------- Start batch worker"
echo ""

${timed} ${bin}/batch-worker $src/project/*.hs

echo "------- Start Buck worker"
echo ""

buck kill
buck build //ops/buck-test/comparison1:comparison1 -j1 -v 2,stderr
buck kill
''
