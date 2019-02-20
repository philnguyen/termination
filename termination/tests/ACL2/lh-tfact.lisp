(defun tfac (x n)
  (if (or (not (integerp n)) (<= n 0))
    x
    (tfac (+ n x) (- n 1))))
