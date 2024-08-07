(lang dune 3.0)
(cram enable)

(name elykseer)

(generate_opam_files true)

(using coq 0.2)

(source
 (github eLyKseeR/elykseer-ml))

(authors "Alexander Diemand <codieplusplus@apax.net>")

(maintainers "Alexander Diemand <codieplusplus@apax.net>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name elykseer)
 (synopsis "Cryptographic Data Archive")
 (description "Data life cycle management; very strong encryption to last into the quantum cryptography era")
 (depends
   (ocaml (>= 5.1.1))
   dune
   yojson
 )
 (tags
  ("elixir for your data"
   "quantum resistant encryption"
   "data life cycle management"
   "data archive")
 )
)

(package
  (name elykseer_base)
  (synopsis "eLyKseeR base support code")
  (depends
    (ocaml (>= 5.1.1))
    (elykseer (= :version))
    unix mlcpp_cstdio mlcpp_filesystem elykseer_crypto)
)

(package
  (name elykseer_utils)
  (synopsis "eLyKseeR utility code")
  (depends
    (ocaml (>= 5.1.1))
    (elykseer (= :version))
    yojson mlcpp_cstdio mlcpp_filesystem elykseer_crypto)
)

