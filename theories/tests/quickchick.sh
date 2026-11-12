#!/bin/sh
# Runs the QuickChick property tests of theories/tests/*QC.v (needs coq-quickchick
# and the compiled theories, e.g. by `make`). Fails if any property fails.
#
# usage: theories/tests/quickchick.sh

set -eu

cd "$(dirname "$0")/../.."

out=$(mktemp -d)
trap 'rm -rf "$out"' EXIT

status=0
for f in theories/tests/*QC.v; do
    echo "== $f"
    # QuickChick extracts into the current directory
    if ! (cd "$out" && coqc -R "$OLDPWD/theories" LXR "$OLDPWD/$f") > "$out/log" 2>&1; then
        cat "$out/log"; status=1; continue
    fi
    grep -v '^Warning\|^\[' "$out/log" | grep -v '^File ' || true
    if grep -q -E '^\*\*\* Failed|Gave up' "$out/log"; then status=1; fi
done
exit $status
