#!/usr/bin/env bash
# Runs the `sh` code blocks of EXAMPLES.md in order, in a fresh directory,
# with the built tools on the PATH; fails on the first failing command.
#   bash test/test_examples.sh [<bindir>]   (default: _build/install/default/bin)

set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
BIN="$(cd "${1:-${ROOT}/_build/install/default/bin}" && pwd)"
WORK="$(mktemp -d)"
trap 'rm -rf "${WORK}"' EXIT

# extract the ```sh blocks
awk '/^```sh$/ {inblock=1; next} /^```$/ {inblock=0; next} inblock {print}' "${ROOT}/EXAMPLES.md" > "${WORK}/examples.sh"

cd "${WORK}"
export PATH="${BIN}:${PATH}"
echo "running $(grep -c . examples.sh) lines from EXAMPLES.md in ${WORK}"
bash -euo pipefail -x examples.sh
echo "all examples ran successfully"
