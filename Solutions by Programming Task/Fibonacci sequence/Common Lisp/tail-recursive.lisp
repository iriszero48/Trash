(defun fib(x)
  (labels ((f (p n i)
             (if (zerop i)
                 p
                 (f n (+ p n) (decf i)))))
    (f 0 1 x)))

(print (loop for i from 0 to 10 collect (fib i)))
