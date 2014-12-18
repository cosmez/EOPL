#lang racket
(define (quibbling words)
  (define (sub-quibbling words)
    (match words
      ['() ""]
      [(list a) a]
      [(list a b) (format "~a and ~a" a b)]
      [(list a b ___) (format "~a, ~a" a (sub-quibbling b))]))
  (format "{~a}" (sub-quibbling words)))

(quibbling '("test" "for" "more" "more"))
(quibbling '("test" "for" "more"))
(quibbling '("test" "more"))
(quibbling '("test"))
(quibbling '())



(define mystr "foo")
(set! mystr (string-append mystr " bar"))
(displayln mystr)


(define-syntax-rule (set-append! str value)
  (set! str (string-append str value)))

(define mymacrostr "foo")
(set-append! mymacrostr " bar")
(displayln mystr)


;there is no built-in way to set! prepend in racket
(define str "foo")
(set! str (string-append "bar " str))
(displayln str)

;but you can create a quick macro to solve that problem
(define-syntax-rule (set-prepend! str value)
  (set! str (string-append value str)))

(define macrostr " bar")
(set-prepend! macrostr "foo")
(displayln macrostr)

(displayln "QuickSelect")

(define k 9)


(define (quickselect A k)
  (define pivot (list-ref A (random (length A))))
  (define A1 (filter (curry > pivot) A))
  (define A2 (filter (curry < pivot) A))
  (cond
    [(<= k (length A1)) (quickselect A1 k)]
    [(> k (- (length A) (length A2))) (quickselect A2 (- k (- (length A) (length A2))))]
    [else pivot]))

(define a '(9 8 7 6 5 0 1 2 3 4))
(display (string-join (map number->string (for/list ([k 10]) (quickselect a (+ 1 k)))) ", "))


