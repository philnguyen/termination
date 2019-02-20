(defun my-merge (xs ys)
  (cond ((and (consp xs) (consp ys))
         (let ((x (car xs))
               (y (car ys)))
           (if (and (integerp x) (integerp y) (< x y))
             (cons x (my-merge (cdr xs) ys))
             (cons y (my-merge xs (cdr ys))))))
        ((consp xs) xs)
        (t ys)))
