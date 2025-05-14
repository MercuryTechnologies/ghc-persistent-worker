import argparse
import json
from pathlib import Path


def main():
    parser = argparse.ArgumentParser(
        description=__doc__, add_help=False, fromfile_prefix_chars="@"
    )
    parser.add_argument(
        "--input",
        required=True,
        type=argparse.FileType("r"),
        help="JSON file that lists toolchain libraries",
    )
    parser.add_argument(
        "--output",
        required=True,
        type=argparse.FileType("w"),
        help="Create required output file.",
    )

    args = parser.parse_args()

    toolchain_libs = json.load(args.input)

    # write toolchains/libs.bzl

    libs = ",\n    ".join(f'"{lib}"' for lib in sorted(toolchain_libs))

    Path("toolchains/libs.bzl").write_text(
        f"""\
# THIS FILE IS AUTO-GENERATED -- DO NOT EDIT --
#
# Note: regenerate with `buck2 bxl haskell/toolchain.bxl:libs`

toolchain_libraries = [
    { libs },
]
"""
    )

    # write toolchains/nix/ghc-with-packages.nix

    libs = "\n  ".join(f'"{ lib }"' for lib in sorted(toolchain_libs))

    Path("ops/ghc-toolchain-libraries.nix").write_text(
        f"""\
# THIS FILE IS AUTO-GENERATED -- DO NOT EDIT --
#
# Note: regenerate with `buck2 bxl haskell/toolchain.bxl:libs`

[
  { libs }
]
"""
    )

    # need to create an output
    print("ok", file=args.output)


if __name__ == "__main__":
    main()
