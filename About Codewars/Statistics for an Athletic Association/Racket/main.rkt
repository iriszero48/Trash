#lang racket
(require racket/string)

(provide stat)

(define (stat strg)
  (let* ([data (sort
                (map (λ(str) (foldl (λ(x y) (+ x (* y 60))) 0 (map string->number (string-split (string-trim str) "|"))))
                     (string-split strg ",")) <)]
         [sec->str (λ(x) (let* ([h (quotient x 3600)]
                                [m (quotient (- x (* h 3600)) 60)]
                                [s (remainder (- x (* h 3600)) 60)])
                           (format "~a|~a|~a"
                                   (~r h #:min-width 2 #:pad-string "0")
                                   (~r m #:min-width 2 #:pad-string "0")
                                   (~r s #:min-width 2 #:pad-string "0"))))])
    (if (equal? strg "") ""
        (format "Range: ~a Average: ~a Median: ~a"
                (sec->str (- (apply max data) (apply min data)))
                (sec->str (inexact->exact (floor (foldl + 0 (map (λ(x) (/ x (length data))) data)))))
                (sec->str (inexact->exact (floor
                                           (foldl + 0
                                                  (map
                                                   (λ(x) (/ (list-ref data (quotient (+ (length data) x) 2)) 2))
                                                   '(-1 0))))))))))
