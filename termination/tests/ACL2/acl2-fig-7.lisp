;; Fig 7
(defun f (x)
  (cond ((or (not (integerp x)) (<= x 1)) 0)
        ((= (mod x 2) 1) (f (+ x 1)))
        (t (+ 1 (f (nonnegative-integer-quotient x 2))))))
