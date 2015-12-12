#lang info

(define collection "static-rename")

(define scribblings '(["scribblings/static-rename.scrbl"]))

(define deps
  '(["base" "6.2"]))
(define build-deps
  '("racket-doc"
    "rackunit-lib"
    "scribble-lib"))
