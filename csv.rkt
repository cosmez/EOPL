#lang racket
(require (planet neil/csv:2:0))

;csv reader configuration
(define csv-reader
  (make-csv-reader-maker
   '((separator-chars            ",")
     (strip-leading-whitespace?  . #t)
     (strip-trailing-whitespace? . #t))))

;tables row structure
(struct masteries (id name tree requirements desc1 desc2 desc3 desc4 desc5) #:transparent)
(struct summoners (id name level desc1 desc2) #:transparent)

;parse the cvs files and get the rows
(define masteries-rows (csv->list (csv-reader (open-input-file "masteries.csv"))))
(define summoners-rows (csv->list (csv-reader (open-input-file "summoners.csv"))))

;check for bad parsing
(if (not (and 
     (andmap (compose (curry = 9) length) masteries-rows) 
     (andmap (compose (curry = 5) length) summoners-rows)))
    (error "error parsing csv files")
    #t)

;;check if mastery exists
(define (mastery-exist? mastery)
  (let* ([id (masteries-id mastery)]
         [query ("SELECT * FROM masteries WHERE id = $1")])
    ('query)))

;import single mastery
(define (import-mastery mastery)
  (define update-mastery '(prepare update))
  (define insert-mastery '(prepare insert))
  (if (mastery-exist? mastery)
      (update-mastery mastery)
      (insert-mastery mastery)))

;first, lets import the masteries
(for/list ([row (in-list masteries-rows)])
  (let [(mastery (apply masteries row))]
    mastery))

;first, now import the summoners
(for/list ([row (in-list summoners-rows)])
  (let [(summoner (apply summoners row))]111
    summoner))