(defun foo (b n m)
  (cond ((not (and (natp n) (natp m))) 0) ; hack
        ((and b (> n 0)) (foo t (1- n) (1+ m)))
        ((and b (<= n 0)) (foo nil 0 (1- m)))
        ((and (not b) (> m 0)) (foo nil (1+ n) (1- m)))
        ((and (not b) (= m 0)) n)))
