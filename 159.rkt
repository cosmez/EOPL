#lang rackjure
;;The best way to see this game and understand the rules is to do some basic research.
;;http://en.wikipedia.org/wiki/Rock-paper-scissors-lizard-Spock
;;https://www.youtube.com/watch?v=iapcKVn7DdY
;;The challenge is to implement a basic game of Rock Paper Scissors Lizard Spock 
;;(to be called RPSLP for short). 
;;Your game will get the human choice. The computer AI will randomly pick a move. 
;;It will compare the results and display the moves and the out come (who wins or if a tie)
(define moves 
  '((scissors cut paper)
   (paper covers rock)
   (rock crushes lizard)
   (lizard poisons spock)
   (spock smashes scissors)
   (scissors decapitate lizard)
   (lizard eats paper)
   (paper disproves spock)
   (spock vaporizes rock)
   (rock crushes scissors)))
(define options (remove-duplicates (map car moves)))

(define (match-move pick1 pick2)
  (filter (λ [move] (and (eq? (first move) pick1) (eq? (third move) pick2))) moves))

(for ([i (in-naturals)])
  (define p1-pick (read))
  (define (show-result result) ;format the output
    (~>> result first flatten (map symbol->string) string-join string-titlecase))
  
  (when (memq p1-pick options)
    (define cpu-pick (list-ref options (random (length options))))
    (printf "Player Picks: ~a\n" p1-pick)
    (printf "CPU Picks: ~a\n" cpu-pick)
    (define result (filter (λ [output] (not (empty? (first output))))
                           `((,(match-move p1-pick cpu-pick) player1 wins)
                             (,(match-move cpu-pick p1-pick) cpu wins)
                             ((same pick) is a draw))))
    (printf "~a\n" (show-result result))))


;;example game
#|
    rock
    Player Picks: rock
    CPU Picks: lizard
    Rock Crushes Lizard Player1 Wins
    paper
    Player Picks: paper
    CPU Picks: lizard
    Lizard Eats Paper Cpu Wins
    lizard
    Player Picks: lizard
    CPU Picks: spock
    Lizard Poisons Spock Player1 Wins
|#