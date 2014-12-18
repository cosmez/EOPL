#lang racket
(define tree '((-1 0 (0 1 (2 3))) (4 5 6 (7 (8 9)))))
(define tree2 '((3 4 1) (1 2 3)))

(define (weight lst)
  (match lst
    [(list (? list? x) xs ...) (+ 1 (weight x) (weight xs))]
    [(list x xs ...) (weight xs)]
    [else 0]))

(define (tree-sum lst)
  (match lst
    [(list (? list? x) xs ...) (+ (tree-sum x) (tree-sum xs))]
    [(list (? number? x) xs ...) (+ x (tree-sum xs))]
    [else 0]))


(define (deep-sort lst)
  (define (less-than? 1st 2nd)
    (cond 
      [(and (number? 1st) (number? 2nd)) (1st . > . 2nd)]
      [(number? 1st) #t]
      [else #f]))
  (define (start-sort lat)
    (cond
      [(number? lat) lat]
      [(ormap list? lat) ((curryr sort less-than?) (map start-sort lat))]
      [else (sort lat less-than?)]))
  (start-sort lst))