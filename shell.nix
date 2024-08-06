{
  system,
  pkgs,
}: let
  bootGHC = "ghc965";
  hsenv = pkgs.haskell.packages.${bootGHC}.ghcWithPackages (p: [
    p.shake
    p.QuickCheck
  ]);
in
  pkgs.mkShell {
    name = "ghcHEAD-shell";
    buildInputs =
      [
        # boot deps
        hsenv
        pkgs.cabal-install
        pkgs.alex
        pkgs.happy
        pkgs.python3

        pkgs.autoconf
        pkgs.automake
        pkgs.m4
        pkgs.less
        pkgs.gmp.dev
        pkgs.gmp.out
        pkgs.glibcLocales
        pkgs.ncurses.dev
        pkgs.ncurses.out
        pkgs.zlib.dev
        pkgs.zlib.out

      ]
      ++ pkgs.lib.optionals pkgs.stdenv.isDarwin [
        pkgs.darwin.apple_sdk.frameworks.Security
        pkgs.darwin.apple_sdk.frameworks.CoreFoundation
        pkgs.darwin.apple_sdk.frameworks.SystemConfiguration
      ];
    CONFIGURE_ARGS = [
      "--with-gmp-includes=${pkgs.gmp.dev}/include"
      "--with-gmp-libraries=${pkgs.gmp}/lib"
      "--with-curses-includes=${pkgs.ncurses.dev}/include"
      "--with-curses-libraries=${pkgs.ncurses.out}/lib"
    ];

    shellHook = ''
      export PS1="\n[ghc-persistent-worker-ghcHEAD:\w]$ \0"

    '';
  }
