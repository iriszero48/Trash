#lang racket
(printf "Celsius = ~a" (inexact->exact (floor (/ (* 5 (- (read) 32)) 9))))
