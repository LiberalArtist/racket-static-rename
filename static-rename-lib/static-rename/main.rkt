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
