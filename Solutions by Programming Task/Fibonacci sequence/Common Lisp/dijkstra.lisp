(defun fib(x)
  (labels
      ((f (a b p q i)
         (if (zerop i)
             b
             (if (evenp i)
                 (f
                  a
                  b
                  (+ (* p p) (* q q))
                  (+ (* q q) (* 2 p q))
                  (floor i 2))
                 (f
                  (+ (* b q) (* a q) (* a p))
                  (+ (* b p) (* a q))
                  p
                  q
                  (- i 1))))))
    (f 1 0 0 1 x)))

(print (loop for i from 0 to 10 collect (fib i)))
