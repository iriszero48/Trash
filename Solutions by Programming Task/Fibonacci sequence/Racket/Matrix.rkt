(require math/matrix)

(define (Fib x)
  (matrix-ref
   (matrix-expt
    (matrix ([1 1] [1 0]))
    (- x 1))
   0 0))
