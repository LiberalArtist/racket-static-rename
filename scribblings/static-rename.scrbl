#lang scribble/manual

@(require (for-label racket/base
                     static-rename)
          scribble/eval)

@(define make-static-rename-eval
   (make-eval-factory '(racket/base static-rename)))

@title{Static Procedure Renaming}

@defmodule[static-rename]

@defform[(define/renamed name-id (head args) body ...+)
         #:grammar
         ([head id
                (head args)]
          [args (code:line arg ...)
                (code:line arg ... @#,racketparenfont{.} rest-id)]
          [arg  arg-id
                [arg-id default-expr]
                (code:line keyword arg-id)
                (code:line keyword [arg-id default-expr])])]{
Defines a procedure, just like @racket[define], but the name of the procedure as produced by
@racket[object-name] is @racket[name-id]. This adjustment is done statically, unlike
@racket[procedure-rename], which changes the name of a procedure at runtime. Therefore, this has a
potential performance benefit.

@(examples #:eval (make-static-rename-eval)
  (define/renamed bar (foo x)
    (* 2 x))
  (foo 3)
  foo
  (object-name foo))}
