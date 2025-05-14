#!/usr/bin/env python3

"""Wrapper script to call nix
"""

import argparse
import os
import subprocess
import sys
import time

def main():
    parser = argparse.ArgumentParser(
        description=__doc__, add_help=False, fromfile_prefix_chars="@"
    )
    parser.add_argument(
        "--buck2-output",
        required=True,
        help="Output link",
    )

    args, nix_args = parser.parse_known_args()
    cmd = ["nix"] + nix_args + ["--out-link", args.buck2_output]

    # Note, Buck2 swallows stdout on successful builds.
    # Redirect to stderr to avoid this.
    returncode = subprocess.call(cmd, stdout=sys.stderr.buffer)

    if returncode != 0:
        return returncode

    does_exist = os.path.exists(args.buck2_output)
    if not does_exist:
        time.sleep(1)
        # Second chance. This is a work-around for the issue [DUX-3095]
        # which is observed on MacOS; nix build --out-link does not create
        # output symlink needed for Buck2, and exit with exit code 0.
        # The issue is largely resolved by running nix build once more.
        # TODO: Fix Nix or wherever the root cause exists.
        returncode2 = subprocess.call(cmd, stdout=sys.stderr.buffer)
        does_exist_2 = os.path.exists(args.buck2_output)
        print("does it exist after re-run? : {}".format(does_exist_2), file=sys.stderr)
        return returncode2

    return 0

if __name__ == "__main__":
    main()
