#!/usr/bin/env bash
# Measures test coverage with bisect_ppx (opam install bisect_ppx):
# runs all tests (unit and command line) on an instrumented build and
# writes a summary and an HTML report to _coverage/.
#   bash test/coverage.sh

set -euo pipefail

cd "$(dirname "$0")/.."
OUT="${PWD}/_coverage"
rm -rf "${OUT}"
mkdir -p "${OUT}"

BISECT_FILE="${OUT}/bisect" dune test --force --instrument-with bisect_ppx

bisect-ppx-report html --coverage-path "${OUT}" -o "${OUT}/html"
bisect-ppx-report summary --coverage-path "${OUT}" --per-file | tee "${OUT}/summary.txt"
