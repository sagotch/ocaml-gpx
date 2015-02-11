# GPX for OCaml

OCaml library for GPX documents manipulation.

Provides a `Gpx` module defining types to match GPX document
structure and functions to navigate between `gpx` type and
`Xml.xml` from `xml-light` OCaml library.

# Installation

Via opam:

    opam install gpx

# Usage

`Gpx.of_xml` returns a `Gpx.gpx` value from a `Xml.xml` value.

`Gpx.to_xml` returns a `Xml.xml` value from a `Gpx.gpx` value.

Example:

    let _ = Xml.parse_in stdin
            |> Gpx.of_xml
            |> Gpx.to_xml
            |> Xml.to_string
            |> print_endline

# Documentation

Browse [online documentation](https://rundat.github.io/ocaml-gpx/).
