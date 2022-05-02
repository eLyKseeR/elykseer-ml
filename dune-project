(lang dune 3.0)

(name elykseer)

(generate_opam_files true)

(using coq 0.2)

(source
 (github username/reponame))

(authors "Alexander Diemand <codieplusplus@apax.net>")

(maintainers "Alexander Diemand <codieplusplus@apax.net>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name elykseer)
 (synopsis "Cryptographic Data Archive")
 (description "Data life cycle management; very strong encryption to last into the quantum cryptography era")
 (depends
   (ocaml (>= 4.12.0))
   dune
 )
 (tags
  ("elixir for your data"
   "quantum resistant encryption"
   "data life cycle management"
   "data archive")
 )
)
; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
