#lang racket
(define (sum-of-squared-digits number (result 0))
  (if (zero? number)
      result
      (sum-of-squared-digits (quotient number 10)
                             (+ result (expt (remainder number 10) 2)))))
(define (happy-path-stream n)
  (stream-cons 
   (sum-of-squared-digits n) 
   (stream-map sum-of-squared-digits (happy-path-stream n))))

(define (happy-list n)
  (for/fold ([so-far '()]) ([s (happy-path-stream n)] #:final (or (= s 1) (memq s so-far)) ) (cons s so-far)))

(define (happy-numbers)
  (stream-filter 
   (λ (happy-path) (= (first happy-path) 1)) 
   (stream-map happy-list (in-naturals))))

(for/list ([i (happy-numbers)] [stop 10]) i)
;((1) (1 10 130 97 49) (1) (1 10) (1 100 68 82) (1 10 13) (1 100 68) (1 10) (1 10 13) (1 10 13 32))