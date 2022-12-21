(defun y (f)
  ((lambda (x) (funcall x x))
   (lambda (y)
     (funcall f (lambda (&rest args)
                  (apply (funcall y y) args))))))

(defun fib (x)
  (funcall (Y (lambda (f)
                (lambda (n)
                  (if (< n 2)
                      n
                      (+
                       (funcall f (- n 1))
                       (funcall f (- n 2)))))))
           x))

(print (loop for i from 0 to 10 collect (fib i)))
