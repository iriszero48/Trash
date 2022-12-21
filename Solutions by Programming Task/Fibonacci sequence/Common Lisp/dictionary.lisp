(defvar *dict* (make-hash-table))
(setf (gethash 0 *dict*) 0)
(setf (gethash 1 *dict*) 1)
(setf (gethash 2 *dict*) 1)

(defun fib(n)
  (let
      ((val (gethash n *dict*)))
    (if val
        val
        (let*
            ((f1 (fib (+ 1 (floor n 2))))
             (f2 (fib (floor (- n 1) 2)))
             (v (if (oddp n)
                    (+ (* f1 f1) (* f2 f2))
                    (- (* f1 f1) (* f2 f2)))))
          (setf (gethash n *dict*) v)))))

(print (loop for i from 0 to 10 collect (fib i)))

