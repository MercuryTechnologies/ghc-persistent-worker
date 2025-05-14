from pathlib import Path
import argparse
import json
import subprocess


def main():
    parser = argparse.ArgumentParser(description=__doc__, fromfile_prefix_chars="@")
    parser.add_argument(
        "--output",
        required=True,
        type=argparse.FileType("w"),
        help="Write package metadata to this file in JSON format.",
    )
    parser.add_argument(
        "--flake", required=True, type=str, help="A flake URL, such as `path:dir/nix`."
    )

    args = parser.parse_args()

    out = subprocess.check_output(
        [
            "nix",
            "eval",
            "--json",
            "--apply", "hs: builtins.map (h: h.drvPath) (builtins.attrValues hs)",
            "--no-update-lock-file",
            "--no-use-registries",
            args.flake,
        ]
    )

    drvs = json.loads(out)

    subprocess.run(
        ["nix", "derivation", "show", "--stdin"],
        input="\n".join(f"{p}^*" for p in sorted(drvs)),
        text=True,
        stdout=args.output,
    )


if __name__ == "__main__":
    main()
