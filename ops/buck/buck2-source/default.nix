# Nix expression to build Buck2 from source.
# Based, in part, on https://github.com/thoughtpolice/buck2-nix/blob/c602d0f44f03310a89f209a322bb122b0d3c557a/buck/nix/buck2/default.nix
#
# To update Buck2:
# - change the `git_rev` and `src.hash` attributes below.
# - copy a fresh `Cargo.lock` from Buck2.
{
  lib,
  darwin,
  fetchFromGitHub,
  installShellFiles,
  fenix,
  makeRustPlatform,
  openssl,
  pkg-config,
  protobuf,
  #rust-bin,
  sqlite,
  stdenv,
  fetchpatch,
}:
let
  # based on Buck2's `rust-toolchain` file.
  #rust-nightly = rust-bin.nightly."2024-07-21".default.override {
  #  extensions = ["llvm-tools-preview" "rustc-dev" "rust-src"];
  #};
  #rustPlatform = makeRustPlatform {
  #  cargo = rust-nightly;
  #  rustc = rust-nightly;
  #};
  rustChannel = fenix.latest; # fenix.beta; #fenix.stable; #fenix.latest;
  rustPlatform = makeRustPlatform {
    inherit (rustChannel) cargo;
    rustc = rustChannel.rustc // {
      # newer nixpkgs' `buildRustPackage` expect rustc to provide `targetPlatforms` and `badTargetPlatforms`
      # see https://github.com/oxalica/rust-overlay/issues/194
      # TODO remove this workaround once fenix is recent enough
      targetPlatforms = lib.platforms.all;
      badTargetPlatforms = [ ];
    };
  };
in
rustPlatform.buildRustPackage rec {
  pname = "buck2";
  git_rev = "2025-02-15";
  version = "git-${git_rev}";

  src = fetchFromGitHub {
    owner = "facebook";
    repo = pname;
    rev = git_rev;
    hash = "sha256-F911L1Auu7DVRV+AUHowkf5jDPHaXAspi3vnL31RPEc=";
  };

  patches = [
    # only upload large files once
    ./pr750.patch
    # OSS write_action_result for local_only cache uploads
    ./pr771.patch
    # Use preferred digest hashing algorithm for action permission checker
    # (https://github.com/facebook/buck2/pull/784)
    ./pr784.patch
    # solve rebuild / cache invalidation issue
    (fetchpatch {
      url = "https://github.com/MercuryTechnologies/buck2/commit/aa3569e50ae58419a35583e16e6b5720c63054b6.diff";
      hash = "sha256-0iwIyuggmf0tFXgS2c53aMnlp7dawfjeN2iqxsMjaDE=";
    })
  ];

  cargoLock = {
    lockFile = ./Cargo.lock;
    allowBuiltinFetchGit = true;
  };

  postPatch = ''
    ln -s ${./Cargo.lock} Cargo.lock
  '';

  nativeBuildInputs = [
    installShellFiles
    protobuf
    pkg-config
  ];
  buildInputs =
    [
      openssl
      sqlite
    ]
    ++ lib.optionals stdenv.isDarwin [
      darwin.apple_sdk.frameworks.CoreFoundation
      darwin.apple_sdk.frameworks.CoreServices
      darwin.apple_sdk.frameworks.IOKit
      darwin.apple_sdk.frameworks.Security
    ];

  BUCK2_BUILD_PROTOC = "${protobuf}/bin/protoc";
  BUCK2_BUILD_PROTOC_INCLUDE = "${protobuf}/include";

  doCheck = false;
  dontStrip = true; # XXX (aseipp): cargo will delete dwarf info but leave symbols for backtraces

  postInstall = ''
    mv $out/bin/buck2     $out/bin/buck
    ln -sfv $out/bin/buck $out/bin/buck2
    mv $out/bin/starlark  $out/bin/buck2-starlark
    mv $out/bin/read_dump $out/bin/buck2-read_dump

    installShellCompletion --cmd buck2 \
      --bash <( $out/bin/buck2 completion bash ) \
      --fish <( $out/bin/buck2 completion fish ) \
      --zsh <( $out/bin/buck2 completion zsh )
  '';

  meta = with lib; {
    description = "Build system, successor to Buck";
    homepage = "https://buck2.build/";
    changelog = "https://github.com/facebook/buck2/blob/main/CHANGELOG.md";
    license = licenses.asl20;
    maintainers = [ ];
    platforms = platforms.linux ++ platforms.darwin;
    mainProgram = "buck2";
  };
}
