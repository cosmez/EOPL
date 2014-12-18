#lang racket

;; The racket/draw libraries provide imperative drawing functions.
;; http://docs.racket-lang.org/draw/index.html
(require racket/draw)
 
(define bm (make-object bitmap% "test.bmp"))

 
; (-> (is-a?/c bitmap%) output-port? void?)
(define (bitmap->ppm bitmap output-port)
  (define height (send bitmap get-height))
  (define width (send bitmap get-width))
  (define buffer (make-bytes (* width height 4))) ;buffer for storing argb data
  (send bitmap get-argb-pixels 0 0 width height buffer) ;copy pixels
  (parameterize ([current-output-port output-port]) 
    (printf "P6\n~a ~a\n255\n" width height) ;header 
    (for ([i (* width height)])
      (define pixel-position (* 4 i))
      ;(when (= (modulo i width) 0) (printf "\n")) 
      (write-byte (bytes-ref buffer (+ pixel-position 1))) ; r
      (write-byte (bytes-ref buffer (+ pixel-position 2))) ; g
      (write-byte (bytes-ref buffer (+ pixel-position 3)))))) ;b

; you can write to a file
(call-with-output-file "image.ppm" #:exists 'replace #:mode 'binary
  (lambda (out)
    (bitmap->ppm bm out)))

;or any other output port 



(define (ppm->jpeg bitmap [jpg-file "output"] [quality 75])
  (define command (format "convert ppm:- -quality ~a jpg:~a.jpg" quality jpg-file))
  (match-define (list in out pid err ctrl)  (process command))
  (bitmap->ppm bitmap out)
  (close-input-port in)
  (close-output-port out))

(ppm->jpeg bm)


(define (read-ppm port)
  (parameterize ([current-input-port port])
    (define magic (read-line))
    (match-define (list w h) (string-split (read-line) " "))
    (define width (string->number w))
    (define height (string->number h))
    (define maxcol (string->number (read-line)))
    (define bm (make-object bitmap% width height))
    (define dc (new bitmap-dc% [bitmap bm]))
    (send dc set-smoothing 'unsmoothed)
    (define (adjust v) (* 255 (/ v maxcol)))
    (for ([x width])
      (for ([y height])
        (define alpha (read-byte))
        (define red (read-byte))
        (define green (read-byte))
        (define blue (read-byte))
        (displayln (list alpha red green blue))
        (define color (make-object color% (adjust red) (adjust green) (adjust blue)))
        (send dc set-pen color 1 'solid)
        (send dc draw-point x y)))
    bm))

(define (image->bmp filename)
  (define command (format "convert ~a ppm:-" filename))
  (match-define (list in out pid err ctrl)  (process command))
  (define bmp (read-ppm in))
  (close-input-port in)
  (close-output-port out)
  bmp)

(image->bmp "input.jpg")