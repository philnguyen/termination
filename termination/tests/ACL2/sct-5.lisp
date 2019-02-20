;; Ex 5
(defun f5 (x y)
  (cond ((endp y) x)
        ((endp x) (f5 y (cdr y)))
        (t (f5 y (cdr x)))))
