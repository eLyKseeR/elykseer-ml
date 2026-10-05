# syntax=docker/dockerfile:1
# run from within docker/:
#   docker buildx build --build-arg BASE_VERSION=1 --build-context elykseer-ml-git=../.git -t codieplusplus/elykseer-ml:latest .
# the base image comes from Dockerfile.base

ARG BASE_VERSION=1

FROM codieplusplus/elykseer-ml:base_${BASE_VERSION} AS build-env

# third-party sources pinned to commits (DEP-1); update deliberately
# v0.3.7
ARG COQ_RECORD_UPDATE_COMMIT=2732f29e98d1bcb0a636c05810873e6e3cfaab25
ARG ML_CPP_CSTDIO_COMMIT=ef6ebd4dcc23ab1536a1293e2fce2fe11ff1cb63
ARG ML_CPP_CHRONO_COMMIT=4919fc3329dacc11853369f25723b9ab1aa114a8
ARG ML_CPP_FILESYSTEM_COMMIT=b3e2039ee7018070c8175aa5490b427077167b82
# v2.2.0
ARG ELYKSEER_CRYPTO_COMMIT=a58a3d4a4adb2605aff78d0ad080a75c6ca11c02

COPY --chown=1000 . /home/coq/

# install into ~/.local: boost-1.83.0, zlib-1.3 (prebuilt, see prebuilt/README.md);
# the tarball must match its recorded checksum
RUN cd prebuilt && grep " dot.local-$(uname -m).tgz\$" SHA256SUMS | sha256sum -c - && cd .. \
 && tar xzf ./prebuilt/dot.local-$(uname -m).tgz

ENV PKG_CONFIG_PATH=/home/coq/.local/lib
ENV BOOST_ROOT=/home/coq/.local
ENV ZLIB_ROOT=/home/coq/.local

RUN eval $(opam env) && cd /tmp/ && git clone https://github.com/tchajed/coq-record-update.git coq-record-update.git && cd coq-record-update.git && git checkout --detach ${COQ_RECORD_UPDATE_COMMIT} && make && make install
RUN eval $(opam env) && cd /tmp/ && git clone https://github.com/CodiePP/ml-cpp-cstdio.git ml-cpp-cstdio.git && cd ml-cpp-cstdio.git && git checkout --detach ${ML_CPP_CSTDIO_COMMIT} && dune build && dune install
RUN eval $(opam env) && cd /tmp/ && git clone https://github.com/CodiePP/ml-cpp-chrono.git ml-cpp-chrono.git && cd ml-cpp-chrono.git && git checkout --detach ${ML_CPP_CHRONO_COMMIT} && dune build && dune install
RUN eval $(opam env) && cd /tmp/ && git clone https://github.com/CodiePP/ml-cpp-filesystem.git ml-cpp-filesystem.git && cd ml-cpp-filesystem.git && git checkout --detach ${ML_CPP_FILESYSTEM_COMMIT} && dune build && dune install
# dev.gnupg.org is unreliable (connection refused / HTTP 429); use the official GitHub mirrors
RUN git config --global url."https://github.com/gpg/".insteadOf "https://dev.gnupg.org/source/"
RUN cd /tmp/ && git clone https://github.com/eLyKseeR/elykseer-crypto.git elykseer-crypto.git && cd elykseer-crypto.git && git checkout --detach ${ELYKSEER_CRYPTO_COMMIT} && git submodule update --init && make -C ext
# C++20 modules need the Ninja generator; GCC 14 crashes (ICE) on elykseer-crypto's modules, so use Clang
USER root
RUN apt update && apt install -y ninja-build clang clang-tools
USER coq
# the prebuilt Boost in ~/.local is static only
RUN eval $(opam env) && cd /tmp/elykseer-crypto.git && sed -i 's/set(Boost_USE_STATIC_LIBS OFF)/set(Boost_USE_STATIC_LIBS ON)/' test/cpp/CMakeLists.txt && cmake -S . -B build -G Ninja --fresh -DCMAKE_BUILD_TYPE=Release -DCMAKE_CXX_COMPILER=clang++ && cmake --build build --target static_lib -j$(nproc) && cd src/ml && dune build -p elykseer_crypto && dune install -p elykseer_crypto

RUN tar cvzf /tmp/coq.home.tgz -C /home/coq --exclude ./prebuilt/\* --exclude ./.local/\* --exclude ./.opam/download-cache/\* .

###############################################################################

# pinned by version and digest, same as Dockerfile.base
FROM debian:trixie-20260918@sha256:9cc080028c43b27d2074d63a5f9caf7166d731494965616c1a6d2827a004585c

# 
LABEL org.opencontainers.image.source=https://github.com/eLyKseeR/elykseer-ml
LABEL org.opencontainers.image.description="formally specified & verified implementation of eLyKseeR in Coq / OCaml"
LABEL org.opencontainers.image.licenses=GPL-3.0-or-later

RUN apt update && apt upgrade -y && apt install -y sudo opam bash git jq binutils texinfo less vim gettext sed curl pkg-config libgmp-dev libffi-dev libcurl4-gnutls-dev nodejs && apt autoremove -y

RUN addgroup --gid 1000 coq && adduser --disabled-password --uid 1000 --gid 1000 coq

COPY sudoers.coq /etc/sudoers.d/coq

COPY --chown=1000 --from=build-env /home/coq/.opam/lxr/bin/irmin /home/coq/.local/bin/irmin

COPY --chown=1000 --from=build-env /tmp/coq.home.tgz /tmp/

COPY --chown=1000 run_test.sh /home/coq/
COPY --chown=1000 setup.sh /home/coq/

USER coq

WORKDIR /home/coq

RUN tar xzf /tmp/coq.home.tgz && rm /tmp/coq.home.tgz

RUN echo 'eval $(opam env)' >> ~/.bashrc

RUN echo 'export PATH=$PATH:${HOME}/.local/bin' >> ~/.bashrc

# the repository's .git is passed in as a named build context:
#   docker buildx build --build-context elykseer-ml-git=../.git ...
COPY --chown=1000 --from=elykseer-ml-git . /tmp/elykseer-ml.git

RUN git clone /tmp/elykseer-ml.git elykseer-ml.git \
 && git -C elykseer-ml.git checkout --detach $(git --git-dir=/tmp/elykseer-ml.git rev-parse HEAD) \
 && rm -rf /tmp/elykseer-ml.git

WORKDIR /home/coq/elykseer-ml.git

RUN eval $(opam env) && coq_makefile -f _CoqProject -o Makefile

# evaluate Coq files and extract code
RUN eval $(opam env) && make

# build libraries and executables with OCaml
RUN eval $(opam env) && dune build && dune install --prefix ${HOME}/.local

RUN strip -s ${HOME}/.local/bin/lxr_*

CMD bash
