FROM alpine:latest

# 
LABEL org.opencontainers.image.source=https://github.com/eLyKseeR/elykseer-ml
LABEL org.opencontainers.image.description="formally specified & verified implementation of eLyKseeR in Coq / OCaml"
LABEL org.opencontainers.image.licenses=GPL-3.0-or-later


RUN addgroup -g 1000 coq \
    && adduser -u 1000 -G coq -s /bin/bash -D coq \
    && apk update \
    && apk add --no-cache opam bash cmake boost-dev git alpine-sdk rsync findutils gmp-dev gpgme-dev pandoc html2text

COPY --chown=1000 . /home/coq/

USER coq

WORKDIR /home/coq

RUN opam init -a -q --bare --disable-sandboxing

RUN opam switch -y create 4.14.1

RUN eval $(opam env) && opam repo add coq-released https://coq.inria.fr/opam/released

RUN eval $(opam env) && opam -y install yojson lwt_ppx sha irmin-git irmin alcotest alcotest-lwt ppx_optcomp

RUN eval $(opam env) && cd /tmp/ && git clone https://github.com/CodiePP/ml-cpp-cstdio.git ml-cpp-cstdio.git && cd ml-cpp-cstdio.git && dune build && dune install
RUN eval $(opam env) && cd /tmp/ && git clone https://github.com/CodiePP/ml-cpp-chrono.git ml-cpp-chrono.git && cd ml-cpp-chrono.git && dune build && dune install
RUN eval $(opam env) && cd /tmp/ && git clone https://github.com/CodiePP/ml-cpp-filesystem.git ml-cpp-filesystem.git && cd ml-cpp-filesystem.git && dune build && dune install
RUN eval $(opam env) && cd /tmp/ && git clone https://github.com/eLyKseeR/elykseer-crypto.git elykseer-crypto.git && cd elykseer-crypto.git && git submodule update --init && cd ext/cryptopp && make && cd ../../BUILD && ./mk_cpp.sh && cmake .

RUN eval $(opam env) && opam -y pin add coq 8.15.1

RUN eval $(opam env) && coq_makefile -f _CoqProject -o Makefile

RUN eval $(opam env) && make


CMD bash

