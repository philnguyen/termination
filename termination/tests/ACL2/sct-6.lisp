;; Ex 6
(defun g6 (c d)
  (if (endp c)
    d
    (g6 (cdr c) (cons (car c) d))))
(defun f6 (a b)
  (if (endp b)
    (g6 a nil)
    (f6 (cons (car b) a) (cdr b))))
