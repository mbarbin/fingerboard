(lang dune 3.17)

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
  (base
   (>= v0.17))
  (patdiff
   (>= v0.17))
  (ppx_compare
   (>= v0.17))
  (ppx_enumerate
   (>= v0.17))
  (ppx_hash
   (>= v0.17))
  (ppx_here
   (>= v0.17))
  (ppx_let
   (>= v0.17))
  (ppx_sexp_conv
   (>= v0.17))
  (ppx_sexp_value
   (>= v0.17))
  (ppxlib
   (>= 0.33))
  (textutils
   (>= v0.17))))

(package
 (name fingerboard-system)
 (synopsis "Package for Fingerboard systems")
 (depends
  (ocaml
   (>= 5.2))
  (base
   (>= v0.17))
  (expect_test_helpers_core
   (>= v0.17))
  (fingerboard
   (= :version))
  (patdiff
   (>= v0.17))
  (ppx_compare
   (>= v0.17))
  (ppx_enumerate
   (>= v0.17))
  (ppx_expect
   (>= v0.17))
  (ppx_hash
   (>= v0.17))
  (ppx_here
   (>= v0.17))
  (ppx_let
   (>= v0.17))
  (ppx_sexp_conv
   (>= v0.17))
  (ppx_sexp_value
   (>= v0.17))
  (ppxlib
   (>= 0.33))
  (textutils
   (>= v0.17))))

(package
 (name fingerboard-tests)
 (synopsis "Tests for fingerboard")
 (depends
  (ocaml
   (>= 5.2))
  (ocamlformat
   (and
    :with-dev-setup
    (= 0.27.0)))
  (base
   (>= v0.17))
  (bisect_ppx
   (and
    :with-dev-setup
    (>= 2.8.3)))
  (expect_test_helpers_core
   (>= v0.17))
  (fingerboard
   (= :version))
  (fingerboard-system
   (= :version))
  (patdiff
   (>= v0.17))
  (ppx_compare
   (>= v0.17))
  (ppx_enumerate
   (>= v0.17))
  (ppx_expect
   (>= v0.17))
  (ppx_hash
   (>= v0.17))
  (ppx_here
   (>= v0.17))
  (ppx_js_style
   (and
    :with-dev-setup
    (>= v0.17)))
  (ppx_let
   (>= v0.17))
  (ppx_sexp_conv
   (>= v0.17))
  (ppx_sexp_value
   (>= v0.17))
  (ppxlib
   (>= 0.33))
  (textutils
   (>= v0.17))
  (sherlodoc
   (and
    :with-doc
    (>= 0.2)))))
