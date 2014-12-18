#lang racket
;;very simple and naive let
(define-syntax-rule (mylet-simple (a b) body ...)
  ((lambda (a) body ...) b))

;;a complete let* definition
(define-syntax my-let
  (syntax-rules ()
    [(my-let ([a b]) body ...) ((lambda (a) body ...) b)]
    [(my-let ([a b] c ...) body ...) ((lambda (a) (my-let (c ...) body ...)) b)]))
;;test
(my-let ([a 10] [b (+ a 20)]) (display a) (display b))

;; clojure do
(define-syntax-rule (do body ...)
  (begin body ...))
;;test
(do (newline) (display "testing") (newline))

;clojure fn macro
;(fn name? [params* ] exprs*)
(define-syntax fn
  (syntax-rules ()
    [(fn name? (params ...) body ...) 
     (define (name? params ...) body ...)]))

(fn test [what?] (display what?))
(test "nothing\n")