(define (Fib x)
  (round (/
          (*
           (expt 2 (- x))
           (-
            (expt (+ 1 (sqrt 5)) x)
            (*
             (expt (- (sqrt 5) 1) x)
             (cos (* pi x)))))
          (sqrt 5))))
