(define (Fib x)
  (if (< x 2)
      x
      (+ (Fib (- x 1))
         (Fib (- x 2)))))
