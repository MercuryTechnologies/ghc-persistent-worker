# Run this with `nix run .#hybrid`
{util}: let

  inherit (util) pkgs;

in pkgs.writeScript "hybrid" ''
#!${pkgs.runtimeShell}

buck clean
buck build //ops/buck-test/hybrid/project/... -v 2,stderr
buck kill
''
