(lang dune 3.0)
(using mdx 0.2)

(name moonpool)
(version 0.6)
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
  (ocaml (>= 4.14))
  dune
  (either (>= 1.0))
  (trace :with-test)
  (trace-tef :with-test)
  (qcheck-core (and :with-test (>= 0.19)))
  (thread-local-storage (and (>= 0.2) (< 0.3)))
  (odoc :with-doc)
  (hmap :with-test)
  picos
  picos_std
  (mdx
   (and
    (>= 1.9.0)
    :with-test)))
 (depopts
   hmap
   (trace (>= 0.6)))
 (tags
  (thread pool domain futures fork-join)))

(package
  (name moonpool-lwt)
  (synopsis "Event loop for moonpool based on Lwt-engine (experimental)")
  (allow_empty)  ; on < 5.0
  (depends
    (moonpool (= :version))
    (ocaml (>= 5.0))
    lwt
    base-unix
    (trace :with-test)
    (trace-tef :with-test)
    (odoc :with-doc)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
