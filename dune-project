(lang dune 3.16)

(name ScaML)

(generate_opam_files true)

(authors
 "Julia Kononova <julickk04@mail.ru>"
 "Danil Slinchuk <danil.slinchuk@gmail.com>")

(maintainers
 "Julia Kononova <julickk04@mail.ru>"
 "Danil Slinchuk <danil.slinchuk@gmail.com>")

(license MIT)

(homepage "https://github.com/RoketFlame/ScaML")

(bug_reports "https://github.com/RoketFlame/ScaML/issues")

(package
 (name ScaML)
 (synopsis "Pretty OCaml compiler")
 (description "Pretty OCaml compiler")
 (depends
  ocaml
  dune
  base
  stdio
  lambda-term
  zed
  lwt
  react
  angstrom
  (monads
   (= dev))
  ppx_deriving
  ppx_expect
  ppx_sexp_conv
  bisect_ppx))
