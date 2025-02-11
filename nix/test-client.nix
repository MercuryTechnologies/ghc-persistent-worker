{pkgs}: let

  script = pkgs.writeScript "test-client" ''
  #!${pkgs.zsh}/bin/zsh
  exec nix develop -c ${./test-client.zsh}
  '';

in {
  inherit script;
}
