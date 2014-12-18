#lang racket
;; is an atom?
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

;; List of atoms?
(define lat?
  (lambda (l)
    (cond
     [(null? l) #t]
     [(atom? (car l)) (lat? (cdr l))]
     [else #f])))

;;;
;;;  CONS THE MAGNIFICENT
;;;

;; Is a a member of l?
(define member?
  (lambda (a l)
    (cond
     [(null? l) #f]
     [else (or (eq? (car l) a) (member? a (cdr l)))])))

;; Remove a from lat
(define rember-old
  (lambda (a lat)
    (cond
     [(null? lat) '()]
     [(eq? a (car lat)) (cdr lat)]
     (else (cons (car lat) (rember-old a (cdr lat)))))))

;; first element of every list in lat
(define firsts
  (lambda (lat)
    (cond
     [(null? lat) '()]
     [else (cons (car (car lat)) (firsts (cdr lat)))])))

;; second element of every list in lat
(define seconds
  (lambda (lat)
    (cond
     [(null? lat) '()]
     [else (cons (car (cdr (car lat))) (seconds (cdr lat)))])))

;; insert new at the right of old element
(define insertR
  (lambda (new old lat)
    (cond
     [(null? lat) '()]
     [(eq? (car lat) old) (cons (car lat) (cons new (cdr lat)))]
     [else (cons (car lat) (insertR new old (cdr lat)))])))

;; insert new at the left of old element
(define insertL
  (lambda (new old lat)
    (cond
     [(null? lat) '()]
     [(eq? (car lat) old) (cons new lat)]
     [else (cons (car lat) (insertL new old (cdr lat)))])))

;; replaces old element with new element
(define subst
  (lambda (new old lat)
    (cond
     [(null? lat) '()]
     [(eq? (car lat) old) (cons new (cdr lat))]
     [else (cons (car lat) (subst new old (cdr lat)))])))

;; replaces o1 or o2 with new element
(define subst2
  (lambda (new o1 o2 lat)
    (cond
     [(null? lat) '()]
     [(or (eq? (car lat) o1) (eq? (car lat) o2)) (cons new (cdr lat))]
     [else (cons (car lat) (subst new o1 o2 (cdr lat)))])))

;; replace all ocurrences of a in lat
(define multirember
  (lambda (a lat)
    (cond
     [(null? lat) '()]
     [(eq? a (car lat)) (multirember a (cdr lat))]
     [else (cons (car lat) (multirember a (cdr lat)))])))

;; insert new at the right of every old
(define multiinsertR
  (lambda (new old lat)
    (cond
     [(null? lat) '()]
     [(eq? old (car lat)) (cons (car lat) (cons new (multiinsertR new old (cdr lat))))]
     [else (cons (car lat) (multiinsertR new old (cdr lat)))])))


;; insert new at the left of every old
(define multiinsertL
  (lambda (new old lat)
    (cond
     [(null? lat) '()]
     [(eq? old (car lat)) (cons new (cons (car lat) (multiinsertL new old (cdr lat))))]
     [else (cons (car lat) (multiinsertL new old (cdr lat)))])))


;;
;;      NUMBER GAMES
;;

;; adds 1 to n
(define add1
  (lambda (n) (+ n 1)))


;; substracts 1 to n
(define sub1
  (lambda (n) (- n 1)))

;; adds m to n
(define o+
  (lambda (n m)
    (cond
     [(zero? m) n]
     [else (add1 (o+ n (sub1 m)))])))

;; substracts m to n
(define o-
  (lambda (n m)
    (cond
     [(zero? m) n]
     [else (sub1 (o- n (sub1 m)))])))

;; checks if lat is a list of numbers
(define tup?
  (lambda (lat)
    (cond
     [(null? lat) #t]
     [(not (and
	    (number? (car lat))
	    (positive? (car lat)))) #f]
     [else (tup? (cdr lat))])))

;; Adds the number in a tup
(define addtup
  (lambda (lat)
    (cond
     [(null? lat) 0]
     [else (o+ (car lat) (addtup (cdr lat)))])))

;; sum n, m times
(define X
  (lambda (n m)
    (cond
     [(zero? m) 0]
     [else (o+ n (X n (sub1 m)))])))

;; sums 2 tups into a tup
(define tup+
  (lambda (tup1 tup2)
    (cond
     [(null? tup1) tup2]
     [(null? tup2) tup1]
     [else (cons (o+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))])))

;; is n bigger than m
(define >
  (lambda (n m)
    (cond
     [(zero? n) #f]
     [(zero? m) #t]
     [else (> (sub1 n) (sub1 m))])))

;; is m bigger than n
(define <
  (lambda (n m)
    (> m n)))

;; is n equal to m
(define =
  (lambda (n m)
    (cond
     [(zero? m) (zero? n)]
     [(zero? n) #f]
     [else (= (sub1 n) (sub1 m))])))

;; n power o m
(define ^
  (lambda (n m)
    (cond
     [(zero? m) 1]
     [else (X n (^ n (sub1 m)))])))

;; divide n by m
(define o/
  (lambda (n m)
    (cond
     [(< n m) 0]
     [else (add1 (o/ (- n m) m))])))

;; gets the length of lat
(define olength
  (lambda (lat)
    (cond
     [(null? lat) 0]
     [else (add1 (olength (cdr lat)))])))

;; picks the n member in lat
(define pick
  (lambda (n lat)
    (cond
     [(zero? (sub1 n)) (car lat)]
     [else (pick (sub1 n) (cdr lat))])))

;; removes the n member in lat
(define rempick
  (lambda (n lat)
    (cond
     [(null? lat) '()]
     [(zero? (sub1 n)) (cdr lat)]
     [else (cons (car lat) (rempick (sub1 n) (cdr lat)))])))

;; remove numbers from lat
(define no-nums
  (lambda (lat)
    (cond
     [(null? lat) '()]
     [(number? (car lat)) (no-nums (cdr lat))]
     [else (cons (car lat) (no-nums (cdr lat)))])))

;; removes non numbers from lat
(define all-nums
  (lambda (lat)
    (cond
     [(null? lat) '()]
     [(not (number? (car lat))) (all-nums (cdr lat))]
     [else (cons (car lat) (all-nums (cdr lat)))])))

;; compares two arguments for equality (also numbers)
(define eqan?
  (lambda (n m)
    (cond
     [(and (number? n) (number? m)) (= n m)]
     [(or (number? n) (number? m)) #f]
     [else (eq? n m)])))

;; counts the number of times an atom a appears in lat
(define occur
  (lambda (a lat)
    (cond
     [(null? lat) 0]
     [(eqan? a (car lat)) (add1 (occur a (cdr lat)))]
     [else (occur a (cdr lat))])))

;; one? returns #t if n is 1
(define one?
  (lambda (n)
    (= n 1)))

;; removes the n member in lat
(define rempick-one?
  (lambda (n lat)
    (cond
     [(null? lat) '()]
     [(one? n) (cdr lat)]
     [else (cons (car lat) (rempick-one? (sub1 n) (cdr lat)))])))



;; Full of Stars

;; removes a in l 
(define rember*
  (lambda (a l)
    (cond
     [(null? l) '()]
     [(atom? (car l))
      (cond
       [(eq? a (car l)) (rember* a (cdr l))]
       [else (cons (car l) (rember* a (cdr l)))])]
     [else (cons (rember* a (car l)) (rember* a (cdr l)))])))

;; inserts new at the right of old in l
(define insertR*
  (lambda (new old l)
    (cond
     [(null? l) '()]
     [(atom? (car l))
      (cond
       [(eq? old (car l)) (cons (car l) (cons new (insertR* new old (cdr l))))]
       [else (cons (car l) (insertR* new old (cdr l)))])]
     [else (cons (insertR* new old (car l)) (insertR* new old (cdr l)))])))

;; find the occurences of a in l
(define occur*
  (lambda (a l)
    (cond
     [(null? l) 0]
     [(atom? (car l))
      (cond
       [(eq? a (car l)) (add1 (occur* a (cdr l)))]
       [else (occur* a (cdr l))])]
     [else (o+ (occur* a (car l)) (occur* a (cdr l)))])))


;; replace a in l
(define subst*
  (lambda (new old l)
    (cond
     [(null? l) '()]
     [(atom? (car l))
      (cond
       [(eq? old (car l)) (cons new (subst* new old (cdr l)))]
       [else (cons (car l) (subst* new old (cdr l)))])]
     [else (cons (subst* new old (car l)) (subst* new old (cdr l)))])))



;; inserts new at the left of old in l
(define insertL*
  (lambda (new old l)
    (cond
     [(null? l) '()]
     [(atom? (car l))
      (cond
       [(eq? old (car l)) (cons new (cons (car l) (insertL* new old (cdr l))))]
       [else (cons (car l) (insertL* new old (cdr l)))])]
     [else (cons (insertL* new old (car l)) (insertL* new old (cdr l)))])))


;; is a member of l?
(define member*
  (lambda (a l)
    (cond
     [(null? l) #f]
     [(atom? (car l))
      (cond
       [(eq? a (car l)) #t]
       [else (member* a (cdr l))])]
     [else (or (member* a (car l)) (member* a (cdr l)))])))


;; find the first atom at the left
(define leftmost
  (lambda (l)
    (cond
     [(atom? (car l)) (car l)]
     [else (leftmost (car l))])))


;; find if two lists are equal
(define eqlist?
  (lambda (l1 l2)
    (cond
     [(or (null? l1) (null? l2)) (and (null? l1) (null? l2))] ;; if both lists are null, #t
     [(and (atom? (car l1)) (atom? (car l2)))
      (cond
       [(eq? (car l1) (car l2)) (and #t (eqlist? (cdr l1) (cdr l2)))]
       [else #f])]
     [else (and (eqlist? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))])))


;; equal that works with lists
(define oequal?
  (lambda (s1 s2)
    (cond
     [(and (atom? s1) (atom? s2)) (eqan? s1 s2)]
     [(or (atom? s1) (atom? s2)) #f]
     [else (eqlist? s1 s2)])))


;; Remove a s-exp from lat
(define rember
  (lambda (a lat)
    (cond
     [(null? lat) '()]
     [(oequal? a (car lat)) (cdr lat)]
     (else (cons (car lat) (rember a (cdr lat)))))))


;;
;;  SHADOWS
;;

;; check if list is an arithmetic expression
(define numbered?
  (lambda (l)
    (cond
     [(atom? l) (number? l)]
     [else (and (numbered? (car l)) (numbered? (car (cdr (cdr l)))))])))


;; evals the arithmetic expression
(define value-old
  (lambda (l)
    (cond
     [(atom? l) l]
     [else ((eval (car (cdr l))) (car l) (car (cdr (cdr l))))])))

;; gets the first element in a s-exp
(define 1st-sub-exp
  (lambda (l)
    (car l)))

;; gets the third element in a s-exp
(define 2nd-sub-exp
  (lambda (l)
    (car (cdr (cdr l)))))

;; gets the second element in a s-exp
(define operator
  (lambda (l)
    (car (cdr l))))

;; gets the second element in a s-exp evaluated
(define operator-eval
  (lambda (l)
    (car (cdr l))))

;; gets the value of a multilevel s-exp
(define value
  (lambda (l)
    (cond
     [(atom? l) l]
     [(eq? (operator l) '+) (o+ (value (1st-sub-exp l)) (value (2nd-sub-exp l)))]
     [(eq? (operator l) 'X) (X (value (1st-sub-exp l)) (value (2nd-sub-exp l)))]
     [(eq? (operator l) '-) (o- (value (1st-sub-exp l)) (value (2nd-sub-exp l)))]
     [(eq? (operator l) '^) (^ (value (1st-sub-exp l)) (value (2nd-sub-exp l)))]
     [(eq? (operator l) '/) (o/ (value (1st-sub-exp l)) (value (2nd-sub-exp l)))]
     )))


;; is this sero? '()
(define sero?
  (lambda (l)
    (null? l)))

;; add another '() to the current list
(define edd1
  (lambda (l)
    (cons '() l)))

;; remove a '() to the current list
(define zub1
  (lambda (l)
    (cdr l)))

;; sums the number of '() in n and m
(define o+s
  (lambda (n m)
    (cond
     [(sero? m) n]
     [else (edd1 (o+s n (zub1 m)))])))


;;;
;;; Friends and Relations
;;;

;; checks if l is a set
(define oset?
  (lambda (l)
    (cond
     [(null? l) #t]
     [(member? (car l) (cdr l)) #f]
     [else (oset? (cdr l))])))

;; converts a list into a set, removing duplicates
(define makeset
  (lambda (l)
    (cond
     [(null? l) '()]
     [(member? (car l) (cdr l)) (makeset (cdr l))]
     [else (cons (car l) (makeset (cdr l)))])))

;; checks if set1 is a subset of set2
(define subset?
  (lambda (set1 set2)
    (cond
     [(null? set1) #t]
     [else (and (member? (car set1) set2) (subset? (cdr set1) set2))])))

;; check if set1 is equal to set2
(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2) (subset? set2 set1))))

;; check if any member of set1 exists in set2
(define intersect?
  (lambda (set1 set2)
    (cond
     [(null? set1) #f]
     [else (or (member? (car set1) set2) (intersect? (cdr set1) set2))])))

;; returns every member of set1 in set2
(define intersect
  (lambda (set1 set2)
    (cond
     [(null? set1) '()]
     [(member? (car set1) set2) (cons (car set1) (intersect (cdr set1) set2))]
     [else (intersect (cdr set1) set2)])))


;; combines every non repeated member of set1 and set2
(define union
  (lambda (set1 set2)
    (cond
     [(null? set2) set1]
     [(member? (car set2) set1) (union set1 (cdr set2))]
     [else (cons (car set2) (union set1 (cdr set2)))])))

;; difference between set1 and set2
(define difference
  (lambda (set1 set2)
    (cond
     [(null? set2) '()]
     [(member? (car set2) set1) (difference set1 (cdr set2))]
     [else (cons (car set2) (difference set1 (cdr set2)))])))

;; checks the intersect of multiple lists
(define intersecall
  (lambda (l-set)
    (cond
      [(null? (cdr l-set)) (car l-set)] 
      [else (intersect  (car l-set) (intersecall (cdr l-set)))])))

;; a-pair? definition (helper function)
(define a-pair?
  (lambda (x)
    (cond
      ((atom? x) #f)
      ((null? x) #f)
      ((null? (cdr x)) #f)
      ((null? (cdr (cdr x))) #t)
      (else #f))))

;; helper functions for readibility
;; first element of a pair
(define first
  (lambda (p)
    (car p)))

;; second element of a pair
(define second
  (lambda (p)
    (car (cdr p))))

;; create a pair of two atoms
(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))

;; check if a list is a fun (list of key value pairs where keys are a set)
(define fun?
  (lambda (rel)
    (oset? (firsts (rel)))))

;; reverse a fun relation
(define revrel
  (lambda (rel)
    (cond
      [(null? rel) '()]
      [else (cons (build (second (car rel)) (first (car rel))) (revrel (cdr rel)))])))
 
;; reverse a pair
(define revpair
  (lambda (p)
    (build (second p) (first p))))

;; cleaner fun reverser
(define revrel2
  (lambda (rel)
    (cond
      [(null? rel) '()]
      [else (cons (revpair (car rel)) (revrel (cdr rel)))])))

;; are the values of a fun a set?
(define fullfun?
  (lambda (rel)
    (oset? (seconds (rel)))))

;; member? but using a special f to compare
(define member-f?
  (lambda (a l f)
    (cond 
      [(null? l) #f]
      [else (or (f (car l) a) (member-f? a (cdr l) f))])))

;; member? but using a special f to compare
(define rember-f
  (lambda (a l f)
    (cond 
      [(null? l) '()]
      [(f (car l) a) (rember-f a (cdr l) f)]
      [else (cons (car l) (rember-f a (cdr l) f))])))

;; rember-f function generated version
(define rember-f2
  (lambda (test?)
    (lambda (a l)
      (cond
       [(null? l) '()]
       [(test? (car l) a) (cdr l)]
       [else (cons (car l) ((rember-f2 test?) a (cdr l)))]))))

;; insertL function generated version
(define insertL-f
  (lambda (test?)
    (lambda (new old lat)
      (cond
       [(null? lat) '()]
       [(test? (car lat) old) (cons new lat)]
       [else (cons (car lat) ((insertL-f test?) new old (cdr lat)))]))))

;; insertR function generated version
(define insertR-f
  (lambda (test?)
    (lambda (new old lat)
      (cond
       [(null? lat) '()]
       [(test? (car lat) old) (cons (car lat) (cons new (cdr lat)))]
       [else (cons (car lat) ((insertR-f test?) new old (cdr lat)))]))))

;; insert-g expects for insert position
(define insert-g
  (lambda (where-f)
    (lambda (new old lat)
      (cond
       [(null? lat) '()]
       [(equal? (car lat) old) (where-f new old lat)]
       [else (cons (car lat) ((insert-g where-f) new old (cdr lat)))]))))
;; returns a function depending on the atom

(define atom-to-function
  (lambda (atom)
    (cond
     [(eq? atom 'x) *]
     [(eq? atom '+) +]
     [(eq? atom '-) -]
     [(eq? atom '/) /]
     [^])))


(define value-af
  (lambda (l)
    (cond
     [(atom? l) l]
      [else ((atom-to-function (operator l)) (value (1st-sub-exp l)) (value (2nd-sub-exp l)))])))

(define operation '(1 + 2))
(1st-sub-exp operation)
(value (1st-sub-exp operation))
(2nd-sub-exp operation)
(value (2nd-sub-exp operation))
(operator operation)
((atom-to-function (operator operation)) 1 2)
((atom-to-function (operator operation)) (value (1st-sub-exp operation)) (value (2nd-sub-exp operation)))

(value-af '(1 + 2))


;; replace all ocurrences of f
(define multiremberT
  (lambda (f)
    (lambda (l)
      (cond
       [(null? l) '()]
       [(f (car l)) ((multiremberT f) (cdr l))]
       [else (cons (car l) ((multiremberT f) (cdr l)))]))))

;; inserts new at the left of oldL and new at the right of oldR
(define multiinsertLR
  (lambda (new oldL oldR lat col)
    (cond
     [(null? lat) (col '() 0 0)]
     [(eq? (car lat) oldL) (cons new
				 (cons oldL
				       (multiinsertLR new oldL oldR (cdr lat)
						      (lambda (li le ri)
							(col (cons new (cons oldL li)) (add1 le) ri)))))]
     [(eq? (car lat) oldR) (cons oldR
				 (cons new
				       (multiinsertLR new oldL oldR (cdr lat)
						      (lambda (li le ri)
							(col (cons oldR (cons new li)) le (add1 ri))))))]
     [else (cons (car lat) (multiinsertLR new oldL oldR (cdr lat)
					  (lambda (li le ri)
							(col (cons (car lat) li) le ri))))])))

;; check if number is even?
(define oeven?
  (lambda (x)
    (= (* (modulo x 2) 2))))

;; returns only even numbers in a multidimensional list
(define evens-only*
  (lambda (l)
    (cond
     [(null? l) '()]
     [(atom? (car l))
      (cond
       [(not (even? (car l))) (evens-only* (cdr l))]
       [else (cons (car l) (evens-only* (cdr l)))])]
     [else (cons (evens-only* (car l)) (evens-only* (cdr l)))])))

;; returns an acumlator with the even list, product of even and sum of uneven numbers
(define evens-only*&co
  (lambda (l col)
    (cond
     [(null? l) (col '() 1 0)]
     [(atom? (car l))
      (cond
       [(not (even? (car l)))
	(evens-only*&co (cdr l)
			(lambda (newlat E U)
			  (col newlat E (+ U (car l)))))]
       [else
	(evens-only*&co (cdr l)
			(lambda (newlat E U)
			  (col (cons (car l) newlat) (* E (car l)) U)))])]
     [else
      (evens-only*&co (car l)
		      (lambda (newlat E U)
			(col (cons
			      (car l)
			      (evens-only*&co
			       (cdr l)
			       (lambda (newlat E U)
				 (col (cdr l) E U)))) E U)))])))


;; looking helper function
(define keep-looking
  (lambda (a el lat)
    (cond
     [(number? el) (keep-looking a (pick el lat) lat)]
     [else (eq? el a)])))

;; find a in lat by lat positions
(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))


;; dunno wtf is this
(define A
  (lambda (n m)
    (cond
     [(zero? n) (add1 m)]
     [(zero? m) (A (sub1 n) 1)]
     [else (A (sub1 n) (A n (sub1 m)))])))

(((lambda (f)
   (lambda (l)
     (cond
      [(null? l) 0]
      [else (add1 (f (cdr l)))])))
 ((lambda (g)
   (lambda (l)
     (cond
      [(null? l) 0]
      [else (add1 (g (cdr l)))]))) '())
 ) '(a b))


;; y combinator theory
(((lambda (f)
   (lambda (l)
     (cond
      [(null? l) 0]
      [else (add1 (f (cdr l)))])))
 ((lambda (g)
   (lambda (l)
     (cond
      [(null? l) 0]
      [else (add1 (g (cdr l)))])))
  ((lambda (h)
    (lambda (l)
      (cond
       [(null? l) 0]
       [else (add1 (h (cdr l)))]))) '()) )) '(a b))


;; still working trough the y combinator
(((lambda (le)
   ((lambda (mk-length)
      (mk-length mk-length))
    (lambda (mk-length)
      (le (lambda (x)
	    ((mk-length mk-length) x))))))
 (lambda (length)
   (lambda (l)
     (cond
      [(null? l) 0]
      (else (add1 (length (cdr l)))))))) '(1 2 3 4))

(define Y
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (x) ((f f) x)))))))

