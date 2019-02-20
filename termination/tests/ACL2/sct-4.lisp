;; Ex 4
(defun p (m n r)
  (cond ((or (not (natp m)) (not (natp n)) (not (natp r))) 0) ; hack
        ((> r 0) (p m (- r 1) n))
        ((> n 0) (p r (- n 1) m))
        (t m)))
