(lang dune 2.7.1)
(name minnesota)
(implicit_transitive_deps false)

(generate_opam_files true)
(source (github drwlrsn/minnesota))
(maintainers "Drew Larson <me@drewlarson.ca>")
(authors "Drew Larson <me@drewlarson.ca>")

(package
 (name minnesota)
 (synopsis "Just a bleeding edge gopher server")
 (description "\
Just a bleeding edge gopher server
")
 (documentation https://drwlrsn.github.io/minnesota/)
 (depends alcotest fmt logs))
