;; Ex 2
(mutual-recursion
 (defun f (i x) (if (endp i) x (g (cdr i) x i)))
 (defun g (a b c) (f a (cons b c))))
