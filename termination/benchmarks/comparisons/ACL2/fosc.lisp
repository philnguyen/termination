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
;; Examples from the first-order size-change paper

;; Ex 1
(defun r1 (ls a)
  (if (endp ls)
    a
    (r1 (cdr ls) (cons (car ls) a))))
(defun my-rev (ls) (r1 ls nil))

;; Ex 2
(mutual-recursion
 (defun f (i x) (if (endp i) x (g (cdr i) x i)))
 (defun g (a b c) (f a (cons b c))))

;; Ex 3
(defun a (m n)
  (cond ((or (not (natp m)) (not (natp n))) 0) ;; hack
        ((= m 0) (+ n 1))
        ((= n 0) (a (- m 1) 1))
        (t (a (- m 1) (a m (- n 1))))))

;; Ex 4
(defun p (m n r)
  (cond ((or (not (natp m)) (not (natp n)) (not (natp r))) 0) ; hack
        ((> r 0) (p m (- r 1) n))
        ((> n 0) (p r (- n 1) m))
        (t m)))

;; Ex 5
(defun f5 (x y)
  (cond ((endp y) x)
        ((endp x) (f5 y (cdr y)))
        (t (f5 y (cdr x)))))

;; Ex 6
(defun g6 (c d)
  (if (endp c)
    d
    (g6 (cdr c) (cons (car c) d))))
(defun f6 (a b)
  (if (endp b)
    (g6 a nil)
    (f6 (cons (car b) a) (cdr b))))#|ACL2s-ToDo-Line|#
