#!/bin/bash

# Get the directory of the script
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

protoc --plugin=protoc-gen-haskell=$(which proto-lens-protoc) \
  -I="$SCRIPT_DIR" --haskell_out="$SCRIPT_DIR/src" \
  "$SCRIPT_DIR/worker.proto" "$SCRIPT_DIR/instrument.proto"

# Imports are generated with re-exported modules, which we currently don't support in the buck2 build.
# Remove the re-exported module prefixes so the original modules are imported.
find "$SCRIPT_DIR/src/Proto" -type f -exec perl -pi -E 's/Data\.ProtoLens\.Runtime\.[^ ]* as //g' {} +
