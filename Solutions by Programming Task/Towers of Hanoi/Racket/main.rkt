#lang racket
(define (TowersOfHanoi ndisks from to via)
  (when (> ndisks 0)
    (TowersOfHanoi (- ndisks 1) from via to)
    (printf "Move disk from ~a to ~a\n" from to)
    (TowersOfHanoi (- ndisks 1) via to from)))

(TowersOfHanoi 5 'left 'middle 'right)
