(defun fib(x)
  (loop
    for p = 0 then n
    and n = 1 then (+ p n)
    repeat (incf x)
    maximize p))

(print (loop for i from 0 to 10 collect (fib i)))

