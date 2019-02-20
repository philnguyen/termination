(mutual-recursion
 (defun g (x) (if (integerp x) (f6 (+ x 1)) 0))
 (defun h (x) (if (integerp x) (f6 (- x 1)) 0))
 (defun f6 (x)
   (cond ((or (not (integerp x)) (= x 0)) 0)
         ((< x 0) (g x))
         (t (h x)))))
