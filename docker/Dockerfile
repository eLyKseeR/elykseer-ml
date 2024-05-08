# DOCKER_BUILDKIT=1 docker build -t codieplusplus/elykseer-ml.arm64:local .

FROM debian:latest  AS build-env

RUN apt update && apt upgrade -y && apt install -y opam bash cmake git binutils texinfo automake autoconf less vim cmake gcc g++ gettext sed curl pandoc pkg-config cppcheck libdigest-sha3-perl libgmp-dev libffi-dev

RUN addgroup --gid 1000 coq && adduser --disabled-password --uid 1000 --gid 1000 coq

COPY --chown=1000 . /home/coq/

USER coq

WORKDIR /home/coq

RUN opam init -a -q --bare --disable-sandboxing

RUN opam switch -y create 5.1.1

RUN eval $(opam env) && opam repo add coq-released https://coq.inria.fr/opam/released

RUN eval $(opam env) && opam -y pin add coq 8.18.0
RUN eval $(opam env) && opam -y install yojson lwt_ppx sha irmin-git irmin alcotest alcotest-lwt ppx_optcomp

# install into ~/.local: boost-1.83.0, zlib-1.3 (prebuilt)
RUN tar xzf ./prebuilt/dot.local-$(uname -m).tgz

ENV PKG_CONFIG_PATH=/home/coq/.local/lib
ENV BOOST_ROOT=/home/coq/.local
ENV ZLIB_ROOT=/home/coq/.local

RUN eval $(opam env) && cd /tmp/ && git clone https://github.com/tchajed/coq-record-update.git coq-record-update.git && cd coq-record-update.git && git checkout v0.3.3 && make && make install
RUN eval $(opam env) && cd /tmp/ && git clone https://github.com/CodiePP/ml-cpp-cstdio.git ml-cpp-cstdio.git && cd ml-cpp-cstdio.git && dune build && dune install
RUN eval $(opam env) && cd /tmp/ && git clone https://github.com/CodiePP/ml-cpp-chrono.git ml-cpp-chrono.git && cd ml-cpp-chrono.git && dune build && dune install
RUN eval $(opam env) && cd /tmp/ && git clone https://github.com/CodiePP/ml-cpp-filesystem.git ml-cpp-filesystem.git && cd ml-cpp-filesystem.git && dune build && dune install
RUN eval $(opam env) && cd /tmp/ && git clone https://github.com/eLyKseeR/elykseer-crypto.git elykseer-crypto.git && cd elykseer-crypto.git && git submodule update --init && cd ext && make && cd ../build && sed -i 's/set(Boost_USE_STATIC_LIBS OFF)/set(Boost_USE_STATIC_LIBS ON)/' ./test/CMakeLists.txt && PKG_CONFIG_PATH=~/.local/lib cmake --fresh -DCMAKE_BUILD_TYPE=Release . && make && cd ../src/ml && dune build && dune install

RUN tar cvzf /tmp/coq.home.tgz -C /home/coq --exclude ./prebuilt/\* --exclude ./.local/\* --exclude ./.opam/download-cache/\* .

###############################################################################

FROM debian:latest

# 
LABEL org.opencontainers.image.source=https://github.com/eLyKseeR/elykseer-ml
LABEL org.opencontainers.image.description="formally specified & verified implementation of eLyKseeR in Coq / OCaml"
LABEL org.opencontainers.image.licenses=GPL-3.0-or-later

RUN apt update && apt upgrade -y && apt install -y sudo opam bash git binutils texinfo less vim gettext sed curl pkg-config libgmp-dev libffi-dev && apt autoremove -y

RUN addgroup --gid 1000 coq && adduser --disabled-password --uid 1000 --gid 1000 coq

COPY sudoers.coq /etc/sudoers.d/coq

COPY --chown=1000 --from=build-env /tmp/coq.home.tgz /tmp/

USER coq

WORKDIR /home/coq

RUN tar xzf /tmp/coq.home.tgz && rm /tmp/coq.home.tgz

RUN git clone https://github.com/eLyKseeR/elykseer-ml.git elykseer-ml.git

WORKDIR /home/coq/elykseer-ml.git

RUN git fetch && git checkout 53_deduplication

RUN eval $(opam env) && coq_makefile -f _CoqProject -o Makefile

# evaluate Coq files and extract code
RUN eval $(opam env) && make

# build libraries and executables with OCaml
RUN eval $(opam env) && dune build && dune install --prefix ${HOME}/.local

RUN strip -s ${HOME}/.local/bin/lxr_*

CMD bash
