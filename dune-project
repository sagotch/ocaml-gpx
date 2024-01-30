(lang dune 3.13)

(name gpx)

(version 1.1.2)

(generate_opam_files true)

(source
 (github sagotch/ocaml-gpx))

(authors "Julien Sagot <contact@sagotch.fr>")

(maintainers "Julien Sagot <contact@sagotch.fr>")

(license MIT)

(package
 (name gpx)
 (synopsis "OCaml type for GPX representation, parser and printer.")
 (depends
  (ocaml
   (>= 4.01))
  dune
  ISO8601
  xml-light))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
