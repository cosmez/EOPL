#lang rackjure
(require racket/set)
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

(display "Rock Paper Scissors Lizard Spock - Part 2 Enhancement\n\n")
(display "/r/dailyprogrammer/comments/23qy19/4232014_challenge_159_intermediate_rock_paper/\n")
(display "Enter [quit] to Quit\n")

(display "Pick Your Option: ")

(define (get-next-pick picks)
  (define sorted-picks
    (sort (map 
           (λ [option] 
             `(,option 
               ,(length (filter (λ [move] (equal? move option)) picks))))
           options) 
          > #:key cadr))
  (cond
    [(empty? sorted-picks) (list-ref options (random (length options)))]
    [else 
     (define top-picks 
       (map first 
            (filter 
             (λ (pick) (equal? (second pick) (cadar sorted-picks))) 
             sorted-picks)))
     (define counter-picks 
       (set-subtract (map first (filter (λ [move] (memq (third move) top-picks)) moves)) top-picks))
     (if (empty? counter-picks)
         (list-ref options (random (length options)))
         (list-ref counter-picks (random (length counter-picks))))]))

(define-values (games _)
  (for/fold 
      ([results '()] [picks '()])
    ([i (in-naturals)] 
     #:break (and (i . > . 0)
                  (or (empty? picks)
                      (equal? (first picks) 'quit))))
    (define p1-pick (read))
    (define (show-result result) ;format the output
      (~>> result first flatten (map symbol->string) string-join string-titlecase))
    (if (memq p1-pick options)
        (let* ([cpu-pick (get-next-pick picks)]
               [result (filter (λ [output] (not (empty? (first output))))
                               `((,(match-move p1-pick cpu-pick) player1 wins)
                                 (,(match-move cpu-pick p1-pick) cpu wins)
                                 ((,p1-pick draws cpu-pick) draw)))])
          (printf "Player Picks: ~a\n" p1-pick)
          (printf "CPU Picks: ~a\n" cpu-pick)
          (printf "~a\n" (show-result result))
          (display "Pick Your Option: ")
          (values (cons (first result) results) (cons p1-pick picks)))
        (values results (cons p1-pick picks)))))

(define (winner-count winner) (count (λ [game] (equal? (fourth (flatten game)) winner)) games))
(define-values 
  (player-wins cpu-wins draws total-games)
  (values (winner-count 'player1) (winner-count 'cpu) (winner-count 'draw) (length games)))

(printf "Games Played: ~a Player Wins: ~a ~a% Computer Wins: ~a ~a% Draws: ~a ~a%\n" 
        total-games
        player-wins (/ (* player-wins 100) total-games)
        cpu-wins (/ (* cpu-wins 100) total-games)
        draws (/ (* draws 100) total-games))

#|
Rock Paper Scissors Lizard Spock - Part 2 Enhancement

/r/dailyprogrammer/comments/23qy19/4232014_challenge_159_intermediate_rock_paper/
Enter [quit] to Quit
Pick Your Option: rock
Player Picks: rock
CPU Picks: spock
Spock Vaporizes Rock Cpu Wins
Pick Your Option: paper
Player Picks: paper
CPU Picks: paper
Paper Draws Cpu-Pick Draw
Pick Your Option: lizard
Player Picks: lizard
CPU Picks: scissors
Scissors Decapitate Lizard Cpu Wins
Pick Your Option: rock
Player Picks: rock
CPU Picks: spock
Spock Vaporizes Rock Cpu Wins
Pick Your Option: lizard
Player Picks: lizard
CPU Picks: paper
Lizard Eats Paper Player1 Wins
Pick Your Option: quit
Games Played: 5 Player Wins: 1 20% Computer Wins: 3 60% Draws: 1 20%
|#