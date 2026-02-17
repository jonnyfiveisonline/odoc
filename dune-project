(lang dune 3.21)

(using dune_site 0.1)

(name odoc)

(documentation "https://ocaml.org/p/odoc")

(source
 (github ocaml/odoc))

(license ISC)

(authors
 "Anton Bachin <antonbachin@yahoo.com>"
 "Daniel B\195\188nzli <daniel.buenzli@erratique.ch>"
 "David Sheets <sheets@alum.mit.edu>"
 "Jon Ludlam <jon@recoil.org>"
 "Jules Aguillon <juloo.dsi@gmail.com>"
 "Leo White <leo@lpw25.net>"
 "Lubega Simon <lubegasimon73@gmail.com>"
 "Paul-Elliot Angl\195\168s d'Auriac <paul-elliot@tarides.com>"
 "Thomas Refis <trefis@janestreet.com>"
 "Arthur Wendling <art.wendling@gmail.com>"
 "Emile Trotignon <emile.trotignon@gmail.com>")

(maintainers
 "Daniel B\195\188nzli <daniel.buenzli@erratique.ch>"
 "Jon Ludlam <jon@recoil.org>"
 "Jules Aguillon <juloo.dsi@gmail.com>"
 "Paul-Elliot Angl\195\168s d'Auriac <paul-elliot@tarides.com>")

(cram enable)

(package
 (name odoc)
 (sites (lib extensions))
 (documentation (depends odoc-driver odoc-parser odoc-md sherlodoc cmdliner)))

(package (name odoc-parser) (documentation (depends odoc)))
(package (name odoc-md))
(package (name odoc-driver) (documentation (depends sherlodoc odoc)))
(package (name odoc-bench) (allow_empty))
(package (name sherlodoc))

(using mdx 0.3)

; Sherlodoc
(using menhir 2.1)
(using directory-targets 0.1)
