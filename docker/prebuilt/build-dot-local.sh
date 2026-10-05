#!/usr/bin/env bash
# Rebuilds dot.local-$(uname -m).tgz: static Boost 1.83.0 and zlib 1.3,
# installed into ~/.local, from upstream sources with verified checksums.
# Run in the base image (codieplusplus/elykseer-ml:base_*) as user coq:
#   bash build-dot-local.sh
# then update SHA256SUMS (the build is not bit-for-bit reproducible).

set -euo pipefail

PREFIX="${HOME}/.local"
WORK="$(mktemp -d)"
trap 'rm -rf "${WORK}"' EXIT
cd "${WORK}"

ZLIB_VERSION=1.3
ZLIB_SHA256=ff0ba4c292013dbc27530b3a81e1f9a813cd39de01ca5e0f8bf355702efa593e
BOOST_VERSION=1.83.0
BOOST_SHA256=6478edfe2f3305127cffe8caf73ea0176c53769f4bf1585be237eb30798c3b8e
BOOST_DIR="boost_${BOOST_VERSION//./_}"

curl -fsSLO "https://zlib.net/fossils/zlib-${ZLIB_VERSION}.tar.gz"
echo "${ZLIB_SHA256}  zlib-${ZLIB_VERSION}.tar.gz" | sha256sum -c -
curl -fsSLO "https://archives.boost.io/release/${BOOST_VERSION}/source/${BOOST_DIR}.tar.bz2"
echo "${BOOST_SHA256}  ${BOOST_DIR}.tar.bz2" | sha256sum -c -

tar xzf "zlib-${ZLIB_VERSION}.tar.gz"
(cd "zlib-${ZLIB_VERSION}" && ./configure --prefix="${PREFIX}" && make -j"$(nproc)" && make install)

tar xjf "${BOOST_DIR}.tar.bz2"
(cd "${BOOST_DIR}" \
  && ./bootstrap.sh --prefix="${PREFIX}" --without-libraries=python,mpi,graph_parallel \
  && ./b2 -j"$(nproc)" link=static variant=release threading=multi \
          -sZLIB_INCLUDE="${PREFIX}/include" -sZLIB_LIBPATH="${PREFIX}/lib" install)

tar czf "${HOME}/dot.local-$(uname -m).tgz" -C "${HOME}" .local
sha256sum "${HOME}/dot.local-$(uname -m).tgz"
