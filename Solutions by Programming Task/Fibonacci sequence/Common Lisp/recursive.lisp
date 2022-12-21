(defun fib (x)
  (if (< x 2)
      x
      (+ (fib (- x 1)) (fib (- x 2)))))

(print (loop for i from 0 to 10 collect (fib i)))
