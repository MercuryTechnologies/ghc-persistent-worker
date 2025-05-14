# Run this with `nix run .#restore`
#
# Test that the make state is restored from the Buck cache when recompiling a changed module after that module had been
# built successfully previously.
# This assumes that the worker process was killed by Buck after the first build concluded, causing the worker in the
# second build to have an empty module graph and HPT for all packages that weren't changed.
{util}: let

  inherit (util) pkgs;

in pkgs.writeScript "restore" ''
#!${pkgs.runtimeShell}

dir=$(mktemp -d --tmpdir=$PWD buck-test-restore-XXX)
name=''${dir##*/}

cleanup()
{
  rm -rf $dir
}
trap cleanup EXIT

mkdir -p $dir
${pkgs.rsync}/bin/rsync -rlt ops/buck-test/restore/project/ $dir/
ln -s $dir $name

buck kill
buck build //$name/... -j12 -v 2,stderr
echo "" >> $name/L3_3.hs
buck build //$name/... -j12 -v 2,stderr
buck kill
''
