(defun fib (n)
  (car
   (reduce
    (lambda (s _) (list (cadr s) (apply '+ s)))
    (loop repeat n collect nil)
    :initial-value '(0 1))))

(print (loop for i from 0 to 10 collect (fib i)))
