;; Ex 3
(defun a (m n)
  (cond ((or (not (natp m)) (not (natp n))) 0) ;; hack
        ((= m 0) (+ n 1))
        ((= n 0) (a (- m 1) 1))
        (t (a (- m 1) (a m (- n 1))))))
