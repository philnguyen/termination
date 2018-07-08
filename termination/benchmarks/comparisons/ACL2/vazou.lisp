; ****************** BEGIN INITIALIZATION FOR ACL2s MODE ****************** ;
; (Nothing to see here!  Your actual file is after this initialization code);

#+acl2s-startup (er-progn (assign fmt-error-msg "Problem loading the CCG book.~%") (value :invisible))
(include-book "acl2s/ccg/ccg" :uncertified-okp nil :dir :system :ttags ((:ccg)) :load-compiled-file nil);v4.0 change

;Common base theory for all modes.
#+acl2s-startup (er-progn (assign fmt-error-msg "Problem loading ACL2s base theory book.~%") (value :invisible))
(include-book "acl2s/base-theory" :dir :system :ttags :all)


#+acl2s-startup (er-progn (assign fmt-error-msg "Problem loading ACL2s customizations book.~%Please choose \"Recertify ACL2s system books\" under the ACL2s menu and retry after successful recertification.") (value :invisible))
(include-book "custom" :dir :acl2s-modes :ttags :all)

#+acl2s-startup (er-progn (assign fmt-error-msg "Problem setting up ACL2s mode.") (value :invisible))

;Settings common to all ACL2s modes
(acl2s-common-settings)
;(acl2::xdoc acl2s::defunc) ;; 3 seconds is too much time to spare -- commenting out [2015-02-01 Sun]

(acl2::xdoc acl2s::defunc) ; almost 3 seconds

; Non-events:
;(set-guard-checking :none)

(acl2::in-package "ACL2S")

; ******************* END INITIALIZATION FOR ACL2s MODE ******************* ;
;$ACL2s-SMode$;ACL2s
(defun my-gcd (a b)
  (if (or (not (integerp b)) (<= b 0))
    a
    (my-gcd b (mod a b))))

(defun tfac (x n)
  (if (or (not (integerp n)) (<= n 0))
    x
    (tfac (+ n x) (- n 1))))

(defun range (lo hi)
  (if (and (integerp lo) (integerp hi) (< lo hi))
    (cons lo (range (+ lo 1) hi))
    nil))

;; map is higher-order

(defun my-merge (xs ys)
  (cond ((and (consp xs) (consp ys))
         (let ((x (car xs))
               (y (car ys)))
           (if (and (integerp x) (integerp y) (< x y))
             (cons x (my-merge (cdr xs) ys))
             (cons y (my-merge xs (cdr ys))))))
        ((consp xs) xs)
        (t ys)))#|ACL2s-ToDo-Line|#
