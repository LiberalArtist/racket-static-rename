#lang racket/base

(require (for-syntax racket/base
                     syntax/parse
                     syntax/parse/lib/function-header))

(provide static-rename define/renamed)

(define-syntax static-rename
  (syntax-parser
    [(_ inferred-name:id expr:expr)
     #'(let ([inferred-name expr]) inferred-name)]))

(define-syntax define/renamed
  (syntax-parser
    [(_ inferred-name:id name:id expr:expr)
     #'(define name (static-rename inferred-name expr))]
    [(_ inferred-name:id header:function-header body:expr ...+)
     #'(define/renamed inferred-name header.name (λ header.args body ...))]))

(module+ test
  (require rackunit
           rackunit/spec)

  (define-syntax inferred-name
    (syntax-parser
      [(_) #`'#,(syntax-local-name)]))

  (describe "static-rename"
    (it "adjusts the inferred name of expressions"
      (check-equal? (static-rename external (inferred-name)) 'external)
      (check-equal? (object-name (static-rename external (λ (x) x))) 'external))

    (it "overrides normally-inferred names"
      (check-equal? (let ([internal (static-rename external (inferred-name))])
                      internal)
                    'external)
      (check-equal? (let ([internal (static-rename external (λ (x) x))])
                      (object-name internal))
                    'external)))

  (describe "define/renamed"
    (it "defines expressions with adjusted inferred names"
      (define/renamed external internal (inferred-name))
      (check-equal? internal 'external))

    (it "defines functions with adjusted inferred names"
      (define/renamed bar (foo a b)
        (+ a b))

      (check-equal? (foo 10 20) 30)
      (check-equal? (object-name foo) 'bar))

    (it "defines functions with rest arguments"
      (define/renamed bar (foo a . rest)
        (cons a (apply + rest)))

      (check-equal? (foo 'nums 1 2 3) '(nums . 6))
      (check-equal? (object-name foo) 'bar))

    (it "defines functions without the adjusted name being in scope"
      (define (bar) 42)
      (define/renamed bar (foo) (bar))

      (check-equal? (foo) 42)
      (check-equal? (object-name foo) 'bar))))
