;; Section 3
(defun f (n m)
  (cond ((not (and (natp n) (natp m))) 0) ; hack
        ((= m 0) n)
        ((= n 0) (f m m))
        (t (f (1- m) (1- n)))))
