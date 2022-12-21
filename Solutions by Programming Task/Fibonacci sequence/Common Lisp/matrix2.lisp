(defun fib (x)
  (if (zerop x)
      0
      (flet ((prev-pow-two (x)
               (if (= (logand x (- x)) x)
                   x
                   (progn
                     (decf x)
                     (mapcar (lambda (i)
                               (setq x (logior x (ash x (- i)))))
                             '(1 2 4 8 16))
                     (incf x)
                     (/ x 2)))))
        (let* ((pow-two (prev-pow-two x))
               (f1v (labels ((f1 (i q r s)
                               (if (< i pow-two)
                                   (f1
                                    (* i 2)
                                    (+ (* q q) (* r r))
                                    (* r (+ q s))
                                    (+ (* r r) (* s s)))
                                   (list i q r s))))
                      (f1 1 1 1 0))))
          (destructuring-bind (i q r s) f1v
            (labels ((f2 (i q r s)
                       (if (< i x)
                           (f2 (incf i) (+ q r) q r)
                           r)))
              (f2 i q r s)))))))

(print (loop for i from 0 to 10 collect (fib i)))
