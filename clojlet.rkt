#lang racket
(define-syntax (let@ stx)
  (syntax-case stx ()
    [(_ (var val) body ...) 
     (unless (identifier? #'var)
       (raise-syntax-error #f "not an identifier" stx #'var))
     #'(let ([var val]) body ...)]
    [(_ (var val rest ...) body ...) 
     (unless (identifier? #'var)
       (raise-syntax-error #f "not an identifier" stx #'var))     
     #'(let ([var val]) (let@ [rest ...] body ...))]))

(let@ [a 10 b 20] (printf "~a ~a" a b))