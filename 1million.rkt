#lang racket

;;converts and sexpr to an html string
;;very basic stuff, just following chicas request
(define (sexpr->html sexpr)
  (define (get-properties e) ;;get the list of properties as vectors
    (string-join ;convert to string
     (map (λ (el) (format " ~a=\"~a\"" (vector-ref el 0) (vector-ref  el 1))) ;;map them as properties
         (filter vector? e)))) ; filter all vectors from list
  (define expr ; clean up party
    (if (list? sexpr) 
        (filter (compose not vector?)  sexpr)  ;remove properties
        sexpr)) ;non list does nothing
  (match expr ;parser
    [(? list? e) ;complete tag
     (let ([tag (car e)]
           [properties (get-properties (cdr e))])
       (format "<~a~a>~a</~a>" tag properties (string-join (map sexpr->html (cdr e))) tag))] 
    [(vector a b) ""] ;ignore properties
    [(? string? e) e] ;match single strings as tag content
    [(? symbol? e) (symbol->string e)])) ;match quotes as html tag name

(sexpr->html '(html 
               (head "title") 
               (body
                (h1 #("id" "title") #("name" "title") "My Page")
                (div #("id" "content") #("style" "text-decoration: none") 
                     "Demo for Chicas"))))