# Run this with `nix run .#hybrid`
{util}: let

  inherit (util) pkgs;

in pkgs.writeScript "hybrid" ''
#!${pkgs.runtimeShell}

buck kill
buck build --config=external_cell_prelude.commit_hash=66f02a682328d91ac88be0b671908ec547ec3036 //ops/buck-test/hybrid/project/... -v 2,stderr
buck kill
''
