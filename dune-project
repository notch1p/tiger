(lang dune 3.16)

(name tiger)

(generate_opam_files true)

(source
 (github notch1p/tiger))

(authors "Evan Gao")

(maintainers "Evan Gao")

(license WTFPL)

(package
 (name tiger)
 (synopsis "Tiger book compiler")
 (depends ocaml dune core menhir ppx_deriving ocolor)
 (tags
  ("compiler" "pldi")))

(using menhir 3.0)
; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
