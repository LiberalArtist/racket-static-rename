#lang info

(define collection "static-rename")
(define version "1.0")

(define scribblings '(["scribblings/static-rename.scrbl"]))

(define deps
  '(["base" #:version "6.2"]))
(define build-deps
  '("racket-doc"
    "rackunit-lib"
    "rackunit-spec"
    "scribble-lib"))
