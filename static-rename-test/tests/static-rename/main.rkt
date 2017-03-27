#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         rackunit
         rackunit/spec
         static-rename)

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
    (check-equal? (object-name foo) 'bar)))
