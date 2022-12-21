(defun fib(x)
  (round
   (/
    (*
     (expt 2 (- x))
     (- (expt (+ 1 (sqrt 5)) x)
        (* (expt (- (sqrt 5) 1) x)
           (cos (* pi x)))))
    (sqrt 5))))

(print (loop for i from 0 to 10 collect (fib i)))
