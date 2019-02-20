(defun bar (v n m)
  (cond ((not (and (natp v) (natp n) (natp m))) 0) ; hack
        ((and (<= v 0) (> n 0)) (bar m m m))
        ((> v 0) 0)
        (t 0)))
