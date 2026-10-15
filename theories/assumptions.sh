#!/bin/sh
# Reports the trust base of the specification in a stable, diff-friendly form.
#
# Compiles theories/Assumptions.v (the other theories must be built already,
# e.g. by `make`) and normalises the output of its `Print Assumptions` commands:
#   - one section per checked constant (fully qualified name, from `Locate`)
#   - only the names of the assumptions, sorted (types are left out)
#   - a final section with the union of all assumptions
#
# usage: theories/assumptions.sh [print|check|update]
#   print   write the report to stdout (default)
#   check   fail if the report differs from theories/Assumptions.expected (used in CI)
#   update  accept the current trust base into theories/Assumptions.expected

set -eu

cd "$(dirname "$0")/.."

expected=theories/Assumptions.expected
mode=${1:-print}

case "$mode" in
    print) ;;
    check)
        actual=$(mktemp)
        trap 'rm -f "$actual"' EXIT
        "$0" print > "$actual"
        if diff -u "$expected" "$actual"; then
            echo "trust base matches $expected"
            exit 0
        fi
        echo "trust base changed: review, then run 'theories/assumptions.sh update' and commit $expected" >&2
        exit 1
        ;;
    update)
        "$0" print > "$expected.tmp"
        mv "$expected.tmp" "$expected"
        echo "updated $expected"
        exit 0
        ;;
    *)
        echo "usage: $0 [print|check|update]" >&2
        exit 2
        ;;
esac

raw=$(mktemp)
err=$(mktemp)
trap 'rm -f "$raw" "$err"' EXIT

# warnings go to stderr and must not end up in the report
if ! coqc -R theories LXR theories/Assumptions.v > "$raw" 2> "$err"; then
    cat "$err" >&2
    exit 1
fi

LC_ALL=C awk '
function flush() {
    if (block == "") return
    print "== " block
    if (closed) print "  (closed under the global context)"
    fflush()
    close(sorter)
    block = ""; closed = 0
}
BEGIN { sorter = "LC_ALL=C sort -u | sed \"s/^/  /\""; block = "" }
/^Constant / { flush(); block = $2; section = ""; next }
/^  \(shorter name/ { next }
/^Closed under the global context/ { closed = 1; next }
/^[A-Z][A-Za-z ]*:$/ { section = $0; sub(/:$/, "", section); next }
/^[^ ]/ {
    if (block == "") next
    entry = section ": " $1
    print entry | sorter
    all[entry] = 1
    next
}
END {
    flush()
    print "== all assumptions"
    fflush()
    for (e in all) print e | sorter
    close(sorter)
}
' "$raw"
