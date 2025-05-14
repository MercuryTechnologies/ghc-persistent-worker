# Run this with `nix run .#three-layers`
{util}: let

  inherit (util) pkgs;

in pkgs.writeScript "three-layers" ''
#!${pkgs.runtimeShell}

buck kill
buck build //ops/buck-test/three-layers/project/... -v 2,stderr
buck kill
''
