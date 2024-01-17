(lang dune 3.12)

(name fingerboard)

(generate_opam_files)

(license MIT)

(authors "Mathieu Barbin")

(maintainers "Mathieu Barbin")

(source
 (github mbarbin/fingerboard))

(documentation "https://mbarbin.github.io/fingerboard/")

(package
 (name fingerboard)
 (synopsis
  "Studying various temperaments and intervals on the cello fingerboard")
 (depends
  (ocaml
   (>= 5.1))
  (base
   (and
    (>= v0.16)
    (< v0.17)))
  (patdiff
   (and
    (>= v0.16)
    (< v0.17)))
  (ppx_jane
   (and
    (>= v0.16)
    (< v0.17)))
  (ppx_js_style
   (and
    (>= v0.16)
    (< v0.17)))
  (stdio
   (and
    (>= v0.16)
    (< v0.17)))
  (textutils
   (and
    (>= v0.16)
    (< v0.17)))))
