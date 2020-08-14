#lang racket
(provide nb-year)

(define (nb-year p0 percent aug p)
  (define (loop p0 year)
    (if (>= p0 p)
        year
        (loop
         (floor (+ p0 (* p0 (/ percent 100)) aug))
         (+ year 1))))
  (loop p0 0))
