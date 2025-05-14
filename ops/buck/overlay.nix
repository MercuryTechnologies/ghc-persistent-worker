final: prev: let

  buck2BuildInputs = with final; [
    buck2
    buck2-source
    bash
    coreutils
    cacert
    gnused
    git
    nix
    jq
    graphviz
    openssh
  ];

in {
  buck2 = prev.callPackage ./buck2 {};
  buck2-source = prev.callPackage ./buck2-source {};
  bash-buck = final.writeShellScriptBin "bash" ''
    export PATH='${final.lib.makeSearchPath "bin" buck2BuildInputs}'
    exec "$BASH" "$@"
  '';
}
