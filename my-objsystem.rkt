#lang racket
(require (for-syntax racket/match racket/syntax syntax/stx))

;;send a [m] message to the [o] object, with optional [a] parameters
(define (send o m . a)
  (apply (o m) a))

;;easier to write
(define-syntax (sendy stx)
  (syntax-case stx ()
    [(_ o m) #'(send o (quote m))]
    [(_ o m a) #'(send o (quote m) a)]))


;;object% is the root object, everything is derived from here
(define object% 
  (λ [#:this [owner-this 'undefined]]
    (define this 'undefined) ;this now  contains a pointer to itself
    (define self 'undefined) ;but self needs to be aware of the parent
    (set! this
          (λ [msg] ;actual body of the object
            (case msg
              ['to-string (λ [] "object%")] 
              ;trying to access a method not implemented in the object
              [else 'undefined-method])))
    ;if a parent this is passed, we use that for self
    (set! self (if (equal? owner-this 'undefined) 
                   this 
                   owner-this))
    this))

(define-syntax (classy stx)
  (syntax-case stx ()
    [(_ id (fields ...)) #'(classy id extends object% (fields ...) (super!))]
    [(_ id extends parent (fields ...)) #'(classy id extends object% (fields ...) (super!))]
    [(_ id extends parent (fields ...) constructor ...)
     (with-syntax ([super! (datum->syntax stx 'super!)])
       #`(begin
           (define id
             (λ [#,@(for/list ([field (syntax->list #'(fields ...))] 
                               #:when (not (pair? (syntax->datum field))))
                      field)
                 #:this [parent-this 'undefined]  ;you can pass a parent this
                 #:mixin [mixin 'undefined]] ;and a mixin
               (define parent-class% parent)
               (define super-object 'undefined) ; ala C++ super
               (define (set-super . params) ;set the parent object
               (set! super-object (apply parent-class% params)))
               (define this 'undefined)
               (define self 'undefined)
               (set! this
                     (λ [msg] 
                       (case msg
                         ['this (λ [] this)]
                         #,@(for/list ([method (syntax->list #'(fields ...))] 
                                       #:when (pair? (syntax->datum method)))
                            (with-syntax ([method-name (car (syntax-e method))]
                                          [method-body (datum->syntax stx (cadr (syntax-e method)))])
                              #`[(quote method-name) method-body]))
                         [else 
                          (cond 
                            [(and (not (equal? 'undefined-method (super-object msg)))) 
                             (super-object msg)]
                            ; i still need to implement the mixins
                            #;[(and (not (equal? mix 'undefined)) 
                                    (not (equal? 'undefined-method (mix msg)))) 
                               (mix msg)]
                            [else 'undefined])])))
               ; here we call the constructor
               ((λ [super!] constructor ...) set-super)
               
               (when (equal? super-object 'undefined) 
                 (error "parent should be initialized in the constructor"))
               ;if a parent this is passed, we use that for self
               (set! self (if (equal? parent-this 'undefined) 
                              this 
                              parent-this))
               this))))]))




(classy 
 line%
 (x y 
    [get-x (λ [] x)]
    [get-y (λ [] y)]
    [get-xy (λ [] `(,x ,y))]))

(classy 
 color-line%
 extends line%
 (x y color
  [get-color (λ [] color)] 
  [set-color (λ [v] (set! color v))])
 ; inity is the constructor
 (printf "constructor: ~a ~a ~a\n" x y color)
 (super! x y))

(define line-obj (line% 20 10))
(define color-line-obj (color-line% 10 20 'red))
(sendy color-line-obj get-xy)

#|
(define color% 
  (λ [color #:this [owner-this 'undefined]]
    (define this 'undefined) ;this now  contains a pointer to itself
    (define self 'undefined) ;but self needs to be aware of the parent
    (set! this
          (λ [msg] ;actual body of the object
            (case msg
              ;jquery style object chaining - mypt.setX(10).getX()
              ['getcolor (λ [] color)] 
              ['setcolor (λ [v] (set! color v) self)]
              ;trying to access a method not implemented in the object
              [else 'undefined-method])))
    ;if a parent this is passed, we use that for self
    (set! self (if (equal? owner-this 'undefined) 
                   this 
                   owner-this))
    this))

;;basic definition of a point in a 2D space class
(define point%
  ;static members
  (let ([counter 0])
    (λ [x y #:this [parent-this 'undefined] #:mixin [mixin 'undefined]] 
      ;constructor
      (begin
        (when (< x 0) (set! x 0))
        (when (< y 0) (set! y 0))
        ;; private members, private int x = 0, y = 0;
        (define-values (initial-x initial-y) (values x y)) 
        ;everytime the constructor is called, we change the value of the static member
        (set! counter (add1 counter))
        (define (square x) (* x x)) ;;private member definition - private int square(int x)
        
        (define this 'undefined) ;this now  contains a pointer to itself
        (define self 'undefined) ;but self needs to be aware of the parent
        
        ;this reference
        (set! this
              (λ [msg] ;actual body of the object
                (case msg
                  ;jquery style object chaining - mypt.setX(10).getX()
                  ['setx (λ [v] (set! x v) self)] 
                  ['sety (λ [v] (set! y v) self)]
                  ['getx (λ [] x)] ;basic accesor mypt.getX()
                  ['gety (λ [] y)]
                  ;method accesing private members
                  ['reset (λ [] (set! x initial-x) (set! y initial-y) self)] 
                  ['points (λ [] counter)]
                  ;this is a basic method definition, it accepts another point
                  ;mypt1.distance(mypt2)
                  ['distance (λ [point2] (sqrt (+ 
                                                (square (- (send point2 'getx) x)) 
                                                (square (- (send point2 'gety) y)))))]
                  ;trying to access a method not implemented in the object
                  [else 'undefined-method])))
        ;if a parent this is passed, we use that for self
        (set! self (if (equal? parent-this 'undefined) 
                       this 
                       parent-this))
        this))))

; subclassing point%
(define point-z%
  (λ [x y z #:parent parent%  #:mixin [mixin% 'undefined]]
    (define super 'undefined)
    (define this 'undefined)
    (define mix 'undefined)
    (set! this 
          (λ [msg]
            (case msg
              ['getz (λ [] z)]
              ['super (λ [] super)] ;ala C++ super
              [else 
               (cond 
                 [(and (not (equal? super 'undefined)) (not (equal? 'undefined-method (super msg)))) 
                  (super msg)]
                 [(and (not (equal? mix 'undefined)) (not (equal? 'undefined-method (mix msg)))) 
                  (mix msg)]
                 [else 'undefined])]))) ;asks parent 
    
    (set! super (parent% x y #:this this)) ;extends %point
    (when (not (equal? mixin% 'undefined))
      (set! mix (mixin% 'blue #:this this)))    
    this))


;object definition
(define pt1 (point% 12 22))
(define pt2 (point% 10 20))

(send pt1 'distance pt2) ;;basic method invocation between instances
(send (send (point% -5 -30) 'setx 10) 'getx) ;;jquery style chaining

;object with a parent class
(define pt3 (point-z% 10 20 30 #:parent point% #:mixin color%))
; we modify x through parent% and get the correct object while chaining
(send (send pt3 'setx 20) 'getz)

;send a message to the mixin, chain and send a message to the object
(send (send pt3 'setcolor 'green) 'getz)
|#

;lets add some syntax
