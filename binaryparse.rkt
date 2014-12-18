#lang racket
(require file/sha1)

;;read n bytes from port and converts to integer
(define (read-integers n port)
  (integer-bytes->integer  (read-bytes n port) #f))

;;defines a 4 bytes integer from a port
(define-syntax-rule (defint name port)
  (define name (read-integers 4 port)))

(define (do-something b g r)
  (list b g r))

(define (parse-bmp bmp)
  (define type (read-bytes 2 bmp))
  (defint size bmp)
  (define reserved (read-bytes 4 bmp))
  (defint offset bmp)
  (defint struct-size bmp)
  (defint width bmp)
  (defint height bmp)
  (define planes (read-integers 2 bmp))
  (define bpp (read-integers 2 bmp))
  (defint compression bmp)
  (defint image-size bmp)
  (defint horizontal-resolution bmp)
  (defint vertical-resolution bmp)
  (defint colors-in-palette bmp)
  (defint important-colors bmp)
  (define (iterate)
    (let ([b (read-byte bmp)]
          [g (read-byte bmp)]
          [r (read-byte bmp)])
      (do-something b g r)
      (if (eof-object? b) #f (iterate))))
  (iterate)
  (list type size  reserved offset struct-size width height 
        planes bpp compression image-size horizontal-resolution vertical-resolution 
        colors-in-palette important-colors))


(call-with-input-file "test.bmp" 
  parse-bmp
  #:mode 'binary)

