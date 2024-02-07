(lang dune 3.0)
(using mdx 0.2)

(name moonpool)
(version 0.5.1)
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
  (trace-tef :with-test)
  (qcheck-core (and :with-test (>= 0.19)))
  (odoc :with-doc)
  (mdx
   (and
    (>= 1.9.0)
    :with-test)))
 (depopts
   (trace (>= 0.6))
   thread-local-storage
   (domain-local-await (>= 0.2)))
 (tags
  (thread pool domain futures fork-join)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
