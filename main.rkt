#lang racket/base

(require (for-syntax racket/base
                     syntax/parse
                     syntax/parse/lib/function-header))

(provide define/renamed)

(define-syntax (define/renamed stx)
  (define (replace-function-name name header)
    (syntax-parse header
      [(_:id . args) #`(#,name . args)]
      [(header . args) #`(#,(replace-function-name name #'header) . args)]))
  (syntax-parse stx
    [(_ name:id header:function-header . rest)
     #`(define header.name
         (let ([name (λ header.params . rest)])
           name))]))

(module+ test
  (require rackunit)

  (test-case "it defines functions with custom dynamic names"
   (define/renamed bar (foo a b)
     (+ a b))

   (check-equal? (foo 10 20) 30)
   (check-equal? (object-name foo) 'bar))

  (test-case "the renamed function is not in scope inside the function body"
   (define (bar) 42)
   (define/renamed bar (foo) (bar))

   (check-equal? (foo) 42)
   (check-equal? (object-name foo) 'bar)))
