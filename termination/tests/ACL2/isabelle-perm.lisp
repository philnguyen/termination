;; Section 5
(defun p (m n r)
  (cond ((not (and (natp m) (natp n) (natp r))) 0) ; hack
        ((< 0 r) (p m (- r 1) n))
        ((< 0 n) (p r (- n 1) m))
        (t m)))
