;; Fig 2
(defun f (x)
  (cond ((or (not (integerp x)) (= x 0)) 0)
        ((< x 0) (f (+ x 1)))
        (t (f (- x 1)))))
(defun dec (x)
  (if (or (not (integerp x)) (<= x 0))
    255
    (- x 1)))
(defun foo (i j)
  (cond ((not (and (natp i) (natp j))) 0)
        ((= i 1) (if (= j 1) 0 (foo (dec j) (dec j))))
        (t (foo (dec i) j))))
