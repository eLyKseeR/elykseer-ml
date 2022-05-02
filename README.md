
Preparations
============

0.) opam switch create $(pwd)/opam-4.14.0 4.14.0

1.) eval $(opam env --set-switch --switch=$(pwd)/opam-4.14.0)


# newest version
opam pin add coq 8.15.1

# more packages
opam repo add coq-released https://coq.inria.fr/opam/released


Code generation
===============

1) run proofs in _Coq_ and extract _ML_ code:
    `make -C lib`

2) build library:
    `dune build`

3) build and run executable:
    `dune exec lxr_backup`
