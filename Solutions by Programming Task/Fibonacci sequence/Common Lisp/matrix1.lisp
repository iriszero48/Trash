(defun matrix-mul (a b)
  (mapcar
   (lambda (r)
     (apply #'mapcar
      (lambda (&rest c)
        (apply #'+
         (mapcar #'* r c)))
      b))
   a))

(defconstant *fib-mat* '((1 1) (1 0)))

(defun fib (n)
  (cadadr
   (reduce #'matrix-mul
           (loop repeat n collect *fib-mat*)
           :initial-value *fib-mat*)))

(print (loop for i from 0 to 10 collect (fib i)))
