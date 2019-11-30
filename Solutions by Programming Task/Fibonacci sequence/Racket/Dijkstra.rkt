(define (Fib n)
  (let f ((a 1) (b 0) (p 0) (q 1) (i n))
    (cond ((= i 0) b)
          ((even? i)
           (f a
                    b
                    (+ (* p p) (* q q))
                    (+ (* q q) (* 2 p q))
                    (/ i 2)))
          (else
           (f (+ (* b q) (* a q) (* a p))
                    (+ (* b p) (* a q))
                    p
                    q
                    (- i 1))))))
