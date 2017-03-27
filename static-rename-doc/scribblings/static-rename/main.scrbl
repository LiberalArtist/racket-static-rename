#lang scribble/manual

@(require (for-label racket/base
                     static-rename)
          scribble/eval)

@(define (refref tag)
   @secref[#:doc '(lib "scribblings/reference/reference.scrbl") tag])

@(define make-static-rename-eval
   (make-eval-factory '(racket/base static-rename)))

@title{Static Renaming}

@defmodule[static-rename]

@defform[(static-rename name-id expr)]{
Equivalent to @racket[expr], but adjusts the statically inferred name for @racket[expr] so that
@racket[(syntax-local-name)] produces @racket['name-id] while expanding @racket[expr]. This allows
adjusting the names of things like procedures, which infer their runtime names based on static
information. Unlike @racket[procedure-rename], however, @racket[static-rename] does not introduce any
additional runtime cost.

For more information, see @refref{infernames}.

@(let ([eval (make-static-rename-eval)])
   (begin0
     (examples #:eval eval
       (define f (λ (x) x))
       f
       (define g (static-rename renamed (λ (x) x)))
       g)
     (close-eval eval)))}

@defform*[[(define/renamed name-id id expr)
           (define/renamed name-id (head args) body ...+)]
          #:grammar
          ([head id
                 (head args)]
           [args (code:line arg ...)
                 (code:line arg ... @#,racketparenfont{.} rest-id)]
           [arg  arg-id
                 [arg-id default-expr]
                 (code:line keyword arg-id)
                 (code:line keyword [arg-id default-expr])])]{
Defines a binding, just like @racket[define], but @racket[expr] is statically renamed using
@racket[static-rename]. Also like @racket[define], @racket[define/renamed] is a shorthand form for
defining a function, which will use @racket[name-id] for the runtime name produced by
@racket[object-name].

@(let ([eval (make-static-rename-eval)])
   (begin0
     (examples #:eval eval
       (define/renamed bar (foo x)
         (* 2 x))
       (foo 3)
       foo
       (object-name foo))
     (close-eval eval)))}
