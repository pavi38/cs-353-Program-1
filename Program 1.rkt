#lang racket

;code would work with as many teams with as many player in them playing as many rounds they want.
;reads in data as list of list
;score-sheet list contains the raw data
;player-scores list contains the processsed data


;return the last of the player so we can group the player according to their last name
(define (get-last-name x)
  (define string->list (string-split x))
  (if (> (length string->list) 1)
      (second string->list)(first string->list)
  )
 )

;return the list of list containing all scores by player in three rounds
;("Frosty Snoman X 7 2  4 5  8 /  3 6  X  X  5 / 9 / 1 8" "Frosty Snoman X X X 5 3  7 /  X  8 /  2 6 X  9 / X" "Frosty Snoman X 7 2 X X  8 / 3 6 5 3 9 / 1 / 3 5")
;name of the team is added to the list as list with only one string '(("oddball") .......)

(define (read-from-file file-name)
  (group-by (lambda (x) (get-last-name x))(file->lines file-name)))
  ;(map string-split (file->lines file-name)))

(define (numeric? str)
  (if(equal? (string->number str) #f) #f #t))

(define (string->num str)
  (cond
    [(numeric? str) (string->number str)]
    [(equal? str "X") 10])
)

;all funcation for scoring rules of the game

(define (strike-score score-sheet)
  (cond
    [(equal? "/" (third score-sheet)) (+ 10 10)]
   [else (+ 10  (string->num (second score-sheet)) (string->num (third score-sheet)))]
   )
 )

(define (spare-score score-sheet)
  (+ 10  (string->num (third score-sheet))))

(define (open-frame-score score-sheet)
  (+ (string->num (first score-sheet)) (string->num (second score-sheet))))

(define (add-score-for-round score-rule score-sheet prv-score)
  (+ prv-score (score-rule score-sheet))
)
(define (10-round-strike? round-score count)
   (and (= count 10) (equal? (first round-score) "X") (> (length round-score) 1))
  )
(define (10-round-spare? round-score count)
  (and (= count 10) (numeric? (first round-score)) (equal? (second round-score) "/") (> (length round-score) 2))
  )

;end


;return a list '(player-name score-for-the-round)
;takes in the list player score for single round
; '("Frosty" "Snoman" "X" "7" "2" "4" "5" "8" "/" "3" "6" "X" "X" "5" "/" "9" "/" "1" "8")  -->   '("Frosty Snoman" 143)

(define (one-round-score round-score [total-round-score '()] [score 0] [counter 0])
  (define count (+ counter 1))
  (cond
    [(empty? round-score) (append total-round-score (list score))]
    
    ;Only first name and last name of the player can have length greater than 1
    [(> (string-length (first round-score)) 1) (one-round-score (rest (rest round-score)) (list (string-append (first round-score) " " (second round-score))))]
    ;make a funcation for the and 
    [ (10-round-strike? round-score count)(one-round-score (rest (rest (rest round-score))) total-round-score (add-score-for-round strike-score round-score score) count)]
    [(10-round-spare? round-score count) (one-round-score (rest (rest (rest round-score))) total-round-score (add-score-for-round spare-score round-score score) count)]
    [(equal? (first round-score) "X") (one-round-score (rest round-score) total-round-score (add-score-for-round strike-score round-score score) count)]
    [(and (numeric? (first round-score)) (equal? (second round-score) "/")) (one-round-score (rest (rest round-score)) total-round-score (add-score-for-round spare-score round-score score) count)]
    [(and (numeric? (first round-score)) (numeric? (second round-score))) (one-round-score (rest (rest round-score)) total-round-score (add-score-for-round open-frame-score round-score score) count)]
    )
 )

;cal the total point by a player in all the  rounds add to player-scores as a list ("player name total point" 123)
;add all player scores to player-scores list

(define (player-score-in-all-rounds score-sheet player-scores [total 0])
    (define player-total-score 0)
    (cond
      [(empty? score-sheet) (append player-scores (list ( list (string-append (first (last player-scores)) " total points") total)))]
      [else
       (let
           ([r-score (one-round-score (string-split (first score-sheet)))])
       (player-score-in-all-rounds (rest score-sheet) (append player-scores (list r-score)) (+ total (second r-score))))]
      )
)

;print the total score of the player in all rounds
;cal the highest score

(define (print-total-score player-scores heighest winner)
       (define total-score (second (first player-scores)))
       (begin (displayln (~a "Total score for three rounds: " total-score "\n"))
               (cond
                 [(empty? heighest) (print-final-score (rest player-scores) (list (first player-scores)) winner)]
                 [(> total-score (second (first heighest))) (print-final-score (rest player-scores) (list (first player-scores)) winner)]
                 [(= total-score (second(first heighest))) (print-final-score (rest player-scores) (cons (first player-scores) heighest) winner)]
                 [else (print-final-score (rest player-scores) heighest winner)]
                 ))
)

;print total team score i.e geek total team score 2524
;find the winning team

(define (print-team-score player-scores heighest winner)
  (define team-score (first player-scores))
  (begin
  (displayln (~a (first team-score) (second team-score) "\n"))
  (if(> (second team-score) (second winner)) (print-final-score (rest player-scores) heighest team-score) (print-final-score (rest player-scores) heighest winner))
  )
 )

;print highest score and the winner

(define (print-heighest heighest winner)
  (cond
   [(empty? heighest) displayln(~a "winner is " (first (string-split (first winner))))]
   [else (begin
    (displayln(~a "\n" (first (first heighest)) " make the highest score of " (second (first heighest)) "\n"))
    (print-heighest (rest heighest) winner)
    )]
  )
 )

;main control funcation for printing everything

(define (print-final-score player-scores [heighest-score '()] [winner '("NaN" 0)] )
  
  (cond
    [(empty? player-scores) (print-heighest heighest-score winner)]
    [(= (length (first player-scores)) 1)  (begin(displayln (~a "Team: " (first (first player-scores)) "\n"))(print-final-score (rest player-scores) heighest-score winner))]
    [(string-suffix? (first (first player-scores)) "total points")(print-total-score player-scores heighest-score winner)]
    [(string-suffix? (first (first player-scores)) "team score ") (print-team-score player-scores heighest-score winner)]
    [else (begin (displayln (~a (first (first player-scores)) " " (second (first player-scores)))) (print-final-score (rest player-scores) heighest-score winner))]
   )
  )

;cal the total team score
;control funcation for logic behind processing the data

(define (bowling-score score-sheet player-scores [team-score 0] [team-name " "])
  (define final-team-score (string-append team-name " Total team score "))
  (cond
    [(empty? score-sheet) (print-final-score (append player-scores (list (list final-team-score team-score))))]  
    
    ;Only name of the teams are in the lists with single item
    ;for instance '("oddball")
    
    [(and (= (length (first score-sheet))  1) (> team-score 0)) (bowling-score (rest score-sheet) (append player-scores (list (list final-team-score team-score) (first score-sheet))) 0 (first (first score-sheet)))]
    [(= (length (first score-sheet))  1)(bowling-score (rest score-sheet) (append player-scores (list (first score-sheet))) 0 (first (first score-sheet)))] 
    [else
     (let
         ([single-player-scores (player-score-in-all-rounds (first score-sheet) player-scores)])
     (bowling-score (rest score-sheet) single-player-scores (+ team-score (last (last single-player-scores))) team-name)
     )]
  )
)

(define (main s)
  (cond
    [(string? s) (main (read-from-file s))]
    [else (bowling-score s '())]
  )
)

;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

(main "scores.txt")