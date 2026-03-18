(lang dune 3.20)

(name fingerboard)

(generate_opam_files)

(license "AGPL-3.0-or-later")

(authors "Mathieu Barbin")

(maintainers "Mathieu Barbin <opensource@mbarbin.org>")

(source
 (github mbarbin/fingerboard))

(documentation "https://mbarbin.github.io/fingerboard/")

(implicit_transitive_deps false)

(package
 (name fingerboard)
 (synopsis "A microtonal geography of the cello fingerboard")
 (depends
  (ocaml
   (>= 5.2))
  (bitv
   (>= 2.1))
  (dyn
   (>= 3.20))
  (ordering
   (>= 3.20))
  (pp
   (>= 2.0.0))
  (print-table
   (>= 0.1.3))))

(package
 (name fingerboard-system)
 (synopsis "Package for Fingerboard systems")
 (depends
  (ocaml
   (>= 5.2))
  (fingerboard
   (= :version))
  (ppx_expect
   (>= v0.17))
  (ppxlib
   (>= 0.33))
  (print-table
   (>= 0.1.3))))

(package
 (name fingerboard-tests)
 (synopsis "Tests for fingerboard")
 (depends
  (ocaml
   (>= 5.2))
  (fingerboard
   (= :version))
  (fingerboard-system
   (= :version))
  (ppx_expect
   (>= v0.17))
  (ppxlib
   (>= 0.33))
  (print-table
   (>= 0.1.3))
  (windtrap
   ;; For the scope of the project the [:with-test] annotation below does not
   ;; match our conventions, as this is test-only package. But without it,
   ;; [opam-dune-lint] complains (it is only used by a dune [test] stanza atm.)
   ;; We wish to continue relying on a clean exit code for [opam-dune-lint] at
   ;; this time, so we added the annotation.
   (and
    :with-test
    (>= 0.1.0)))))

(package
 (name fingerboard-dev)
 (synopsis "Package to regroup dev targets, documentation, and more")
 (allow_empty)
 (depends
  (ocaml
   (>= 5.2))
  (ocamlformat
   (= 0.28.1))
  (bisect_ppx
   (>= 2.8.3))
  (fingerboard
   (= :version))
  (fingerboard-system
   (= :version))
  (fingerboard-tests
   (= :version))
  (ppx_js_style
   (>= v0.17))
  (ppxlib
   (>= 0.33))
  (sherlodoc
   (>= 0.2))))
