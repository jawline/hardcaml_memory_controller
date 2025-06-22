(lang dune 2.9)
(name hardcaml_memory_controller)
(generate_opam_files true)
(source
 (github jawline/hardcaml_risc_v))
(authors "Blake Loring")
(maintainers "Blake Loring")
(package
 (name hardcaml_memory_controller)
 (synopsis "A multi-channel memory controller in Hardcaml")
 (description "A multi-channel memory controller in Hardcaml with support for multiple memory types via a generic interface")
 (depends ocaml dune)
 )

