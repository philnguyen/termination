(defun range (lo hi)
  (if (and (integerp lo) (integerp hi) (< lo hi))
    (cons lo (range (+ lo 1) hi))
    nil))
