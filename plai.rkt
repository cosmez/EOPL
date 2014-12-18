#lang plai-typed
(define-type ArithS
  [numS (n : number)]
  [plusS (l : ArithS) (r : ArithS)]
  [bminusS (l : ArithS) (r : ArithS)]
  [multS (l : ArithS) (r : ArithS)])

(define-type ExprC
  [numC (n : number)]
  [varC (s : symbol)]
  [appC (fun : ExprC) (arg : ExprC)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [lamC (arg : symbol) (body : ExprC)]
  [setC (var : symbol) (v : ExprC)]
  [seqC (b1 : ExprC) (b2 : ExprC)])

(define-type Value
  [numV (n : number)]
  [closV (arg : symbol) (body : ExprC) (env : Env)])


(define-type-alias Location number)

(define-type Binding
  [bind (name : symbol) (val : Location)])

(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)

(define-type Storage
  [cell (location : Location) (val : Value)])

(define-type-alias Store (listof Storage))
(define mt-store empty)

(define (override-store [what : Storage] [where : Store]) : Store
  (cond 
    [(empty? where)  (cons what where)] ;; add new location
    [(equal? (cell-location what) (cell-location (first where))) 
     (cons what (rest where))] ;;override location if found
    ;;iterate over locations
    [else (cons (first where) (override-store what (rest where)))]))

(define-type Result
  [v*s (v : Value) (s : Store)])


(define (desugar [as : ArithS]) : ExprC
  (type-case ArithS as
    [numS (n) (numC n)]
    [plusS (l r) (plusC (desugar l)
                        (desugar r))]
    [multS (l r) (multC (desugar l)
                        (desugar r))]
    [bminusS (l r) (plusC (desugar l)
                          (multC (numC -1) (desugar r)))]))

(define (parse [s : s-expression] ) : ExprC
  (cond
    [(s-exp-number? s) (numC (s-exp->number s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         [(+) 
          (plusC (parse (second sl)) (parse (third sl)))]
         [(*) 
          (multC (parse (second sl)) (parse (third sl)))]
         [(λ) 
          (lamC (s-exp->symbol (second sl)) (parse (third sl)))]
         [else (error 'parse "invalid list input")]))]
    [else (error 'parse "invalid input")]))



(define (num+ [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r))
     (numV (+ (numV-n l) (numV-n r)))]
    [else
     (error 'num+ "one argument was not a number")]))

(define (num* [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r))
     (numV (* (numV-n l) (numV-n r)))]
    [else
     (error 'num* "one argument was not a number")]))


(define (lookup [for : symbol] [env : Env]) : Location
  (cond
    [(empty? env) (error 'lookup (string-append (symbol->string for)  " name not found"))]
    [else (cond 
            [(symbol=? for (bind-name (first env))) (bind-val (first env))]
            [else (lookup for (rest env))])]))

(define (fetch [loc : Location] [sto : Store]) : Value
  (cond
    [(empty? sto) (error 'fetch " location not found")]
    [else (cond 
            [(equal? loc (cell-location (first sto))) (cell-val (first sto))]
            [else (fetch loc (rest sto))])]))

(define new-loc
  (let ([n (box 0)])
    (lambda ()
      (begin
        (set-box! n (add1 (unbox n)))
        (unbox n)))))

;;; main interpreter
(define (interp [expr : ExprC] [env : Env] [sto : Store]) : Result
  (type-case ExprC expr
    [numC (n) (v*s (numV n) sto)]
    [varC (n) (v*s (fetch (lookup n env) sto) sto)]
    [lamC (a b) (v*s (closV a b env) sto)]
    [appC (f a)
          (type-case Result (interp f env sto)
            [v*s (v-f s-f)
                 (type-case Result (interp a env s-f)
                   [v*s (v-a s-a)
                        (let ([where (new-loc)])
                          (interp (closV-body v-f)
                                  (extend-env (bind (closV-arg v-f)
                                                    where)
                                              (closV-env v-f))
                                  (override-store (cell where v-a) s-a)))])])]
    [setC (var val) (type-case Result (interp val env sto)
                      [v*s (v-val s-val)
                           (let ([where (lookup var env)])
                             (v*s v-val
                                  (override-store (cell where v-val)
                                                  s-val)))])]
    [plusC (l r) (type-case Result (interp l env sto)
                   [v*s (v-l s-l)
                        (type-case Result (interp r env s-l)
                          [v*s (v-r s-r)
                               (v*s (num+ v-l v-r) s-r)])])]
    [multC (l r) (type-case Result (interp l env sto)
                   [v*s (v-l s-l)
                        (type-case Result (interp r env s-l)
                          [v*s (v-r s-r)
                               (v*s (num* v-l v-r) s-r)])])]    
    [seqC (b1 b2) (type-case Result (interp b1 env sto)
                    [v*s (v-b1 s-b1)
                         (interp b2 env s-b1)])]))





#|
(test (interp (plusC (numC 10) (appC (lamC '_ (numC 5)) (numC 10))) mt-env) (numV 15))
(test/exn (interp (appC (lamC 'x (appC (lamC 'y (plusC (idC 'x) (idC 'y))) (numC 4))) (numC 3)) 
                  mt-env)
          "name not found")


(test (parse '(+ (* 1 2) (+ 2 3))) (plusC (multC (numC 1) (numC 2)) (plusC (numC 2) (numC 3))))
(test (interp (parse '(+ (* 1 2) (+ 2 3))) mt-env function-definitions) 7)
(test (interp (appC 'double (appC 'double (appC 'const5 (numC 5)))) mt-env function-definitions) 20)
(test (interp (parse '(if 0 5 -5)) mt-env function-definitions) -5)
(interp (appC (lamC 'x  (lamC 'y (plusC (idC 'x) (idC 'y)))) (numC 10))  mt-env)
|#
