
Preparations
============

0.) `opam switch create $(pwd)/opam-4.14.0 4.14.0`

1.) `eval $(opam env --set-switch --switch=$(pwd)/opam-4.14.0)`


# newest version
`opam pin add coq 8.15.1`

# more packages
`opam repo add coq-released https://coq.inria.fr/opam/released`

opam install yojson lwt_ppx sha

Code generation
===============

0) create _Coq_'s Makefile
    `coq_makefile -f _CoqProject -o Makefile`

1) run proofs in _Coq_ and extract _ML_ code:
    `make`

2) build library:
    `dune build`

3) build and run executable (shows help):
    `dune exec lxr_backup -- -h`

Docker image
============

docker build -f Dockerfile -t codieplusplus/elykseer-ml:local .
docker run --rm -it codieplusplus/elykseer-ml:local


#### experimenting with multiarch building

`docker buildx build --platform linux/amd64,linux/arm64 -t codieplusplus/elykseer-ml:latest --push .`

