;; Ex 1
(defun r1 (ls a)
  (if (endp ls)
    a
    (r1 (cdr ls) (cons (car ls) a))))
(defun my-rev (ls) (r1 ls nil))
