#!/usr/bin/env bash

set -e

cd ${HOME}

mkdir -vp ${HOME}/elykseer.chunks
mkdir -vp ${HOME}/elykseer.db

cat << EOF > irmin.yml
root: ${HOME}/elykseer.db
store: git
contents: json-value
EOF

irmin init -v

echo
echo "all setup."
