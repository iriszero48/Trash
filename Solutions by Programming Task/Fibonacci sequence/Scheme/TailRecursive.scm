(define (Fib x)
  (let f ((p 1) (n 0) (i x))
    (if (= i 0)
        n
        (f (+ p n) p (- i 1)))))
