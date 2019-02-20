(defun my-gcd (a b)
  (if (or (not (integerp b)) (<= b 0))
    a
    (my-gcd b (mod a b))))
