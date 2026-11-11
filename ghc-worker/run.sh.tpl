#!/usr/bin/env bash
set -euo pipefail
DIR="$(cd "$(dirname "$0")" && pwd)"
# Needed so test output displays correctly
export LANG=C.UTF-8

TMPBASE=$(mktemp --tmpdir=/tmp -d buck_test_sandbox.XXXXXX)
export TMPDIR="$TMPBASE"
export TMP="$TMPBASE"
export TEMP="$TMPBASE"

$DIR/%exe% "$@"
