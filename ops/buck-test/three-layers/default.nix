# Run this with `nix run .#three-layers`
{util}: let

  inherit (util) pkgs;

in pkgs.writeScript "three-layers" ''
#!${pkgs.runtimeShell}

rm /tmp/buck2_worker /tmp/ghc-persistent-worker -rf
pkill -9 -f 'worker --make'
buck clean
buck build //ops/buck-test/three-layers/project/... -j1 -v 2,stderr
''
