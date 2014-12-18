#lang racket
(require net/url)

;;process level runes
(define (process-runes url)
  (define content (port->string (get-pure-port (string->url url) #:redirections 0)))
  (define ids (map string->number (regexp-match* #px"rune/((\\d+).png)" content #:match-select third)))
  (define names (regexp-match* #px"\\\"rune_name\\\">(.+?)\\</td>" content #:match-select second))
  (define descriptions (regexp-match* #px"\\\"rune_description\\\">(.+?)\\</td>" content #:match-select second))
  (map list ids names descriptions))

;join and sort the runes
(define (get-all-runes)
  (let ((level-3 (process-runes "http://euw.leagueoflegends.com/es/runes/3"))
        (level-2 (process-runes "http://euw.leagueoflegends.com/es/runes/2"))
        (level-1 (process-runes "http://euw.leagueoflegends.com/es/runes/1")))
    (sort (append level-1 level-2 level-3)  < #:key car)))



;download the rune images
(define rune-partial-url "http://euw.leagueoflegends.com/sites/default/files/game_data/3.5.0.2/content/rune/")
(define storage "D:\\Temp\\tmp\\images")


(define (save-port location port)
  (call-with-output-file location #:mode 'binary #:exists 'replace 
    (lambda (out)
      (copy-port port out))))

;;(curry save-rune rune) (curry copy-port ) port->bytes get-pure-port  string->url
(define (download-rune-images rune-list)
  (for ([rune rune-list])
    (let* ([name ((compose (curryr string-append ".png") number->string first) rune)]
           [url (string-append rune-partial-url name)]
           [local (string-append storage "\\runes\\" name)]
           [port ((compose get-pure-port string->url) url)])
      (save-port local port)))
  'download-rune-images-done)


;(download-rune-images (get-all-runes))
;;https://github.com/rwarasaurus/league-of-legends-database/raw/master/items/3003.png
;((compose (curry save-port "D:\\Temp\\tmp\\images\\3003.png") get-pure-port string->url) "https://github.com/rwarasaurus/league-of-legends-database/raw/master/items/3003.png")
