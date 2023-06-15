(lang dune 3.0)
(using mdx 0.2)

(name moonpool)
(version 0.2)
(generate_opam_files true)
(source
 (github c-cube/moonpool))
(authors "Simon Cruanes")
(maintainers "Simon Cruanes")
(license MIT)

;(documentation https://url/to/documentation)

(package
 (name moonpool)
 (synopsis "Pools of threads supported by a pool of domains")
 (depends
  (ocaml (>= 4.08))
  dune
  (either (>= 1.0))
  (trace :with-test)
  (odoc :with-doc)
  (mdx
   (and
    (>= 1.9.0)
    :with-test)))
 (tags
  (thread pool domain)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
