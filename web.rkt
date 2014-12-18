#lang racket
(require web-server/servlet web-server/servlet-env web-server/servlet/web)

;; initial handler
(define (start request)
  (blog-dispatch request))

;; here we define the dispatching rules
(define-values (blog-dispatch blog-url)
  (dispatch-rules
   [("") main-page]
   [("api" (integer-arg)) api]
   [("sum" (integer-arg) (integer-arg)) sum]
   [else main-page]))

;; main page handler
(define (main-page req) 
  (response/xexpr
   '(html
     (head (title "My Blog"))
     (body (h1 "Under construction")))))

;; url for handling single subdirectory
(define (api req p) 
  (response/xexpr
   `(html
     (head (title "Number"))
     (body (h1 ,(number->string p))))))

;; url for handling multiple directories
(define (sum req a b) 
  (response/xexpr
   `(html
     (head (title "Suma"))
     (body (h1 ,(number->string (+ a b)))))))

;; run the server
(serve/servlet blog-dispatch
               #:launch-browser? #t
               #:quit? #f
               #:listen-ip #f
               #:port 8000
               #:servlet-path "/"
               #:servlet-regexp #rx""
               )