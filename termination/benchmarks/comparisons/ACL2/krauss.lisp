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
;; Section 3
(defun f (n m)
  (cond ((not (and (natp n) (natp m))) 0) ; hack
        ((= m 0) n)
        ((= n 0) (f m m))
        (t (f (1- m) (1- n)))))

;; Section 5
(defun p (m n r)
  (cond ((not (and (natp m) (natp n) (natp r))) 0) ; hack
        ((< 0 r) (p m (- r 1) n))
        ((< 0 n) (p r (- n 1) m))
        (t m)))

(defun foo (b n m)
  (cond ((not (and (natp n) (natp m))) 0) ; hack
        ((and b (> n 0)) (foo t (1- n) (1+ m)))
        ((and b (<= n 0)) (foo nil 0 (1- m)))
        ((and (not b) (> m 0)) (foo nil (1+ n) (1- m)))
        ((and (not b) (= m 0)) n)))

(defun bar (v n m)
  (cond ((not (and (natp v) (natp n) (natp m))) 0) ; hack
        ((and (<= v 0) (> n 0)) (bar m m m))
        ((> v 0) 0)
        (t 0)))

;; poly? ::= intp
;;        | (cons/c 'inj (cons/c intp poly?))
;;        | (cons/c 'X (cons/c poly? (cons/c intp poly?)))
(defun Pinj (i p) (cons 'inj (cons i p)))
(defun PX (p i q) (cons 'X (cons p (cons i q))))
(defun Pcp (x) (integerp x))
(defun Pinjp (x)
  (and (consp x)
       (equal 'inj (car x))
       (consp (cdr x))
       (integerp (car (cdr x)))))
(defun PXp (x)
  (and (consp x)
       (equal 'X (car x))
       (consp (cdr x))
       (consp (cdr (cdr x)))
       (integerp (car (cdr (cdr x))))))
(defun Pinj-0 (x) (car (cdr x)))
(defun Pinj-1 (x) (cdr (cdr x)))
(defun PX-0 (x) (car (cdr x)))
(defun PX-1 (x) (car (cdr (cdr x))))
(defun PX-2 (x) (cdr (cdr (cdr x))))
;; add : poly? poly? -> intp
(defun add (l r)
  (cond
   ((and (Pcp l) (Pcp r))
    (+ l r))
   ((and (Pcp l) (Pinjp r))
    (Pinj (Pinj-0 r) (add (Pinj-1 r) l)))
   ((and (Pcp l) (PXp r))
    (PX (PX-0 r) (PX-1 r) (add (PX-2 r) l)))
   ((and (Pinjp l) (Pinjp r))
    (let ((x (Pinj-0 l))
          (p (Pinj-1 l))
          (y (Pinj-0 r))
          (q (Pinj-1 r)))
      (cond ((= x y) (Pinj x (add p q)))
            ((< y x) (Pinj y (add (Pinj (- x y) p) q)))
            (t (add r l)))))
   ((and (Pinjp l) (PXp r))
    (let ((x (Pinj-0 l))
          (p (Pinj-1 l))
          (q (PX-0 r))
          (y (PX-1 r))
          (r (PX-2 r))) ; shadowing, careful
      (cond ((= x 0) (add p (PX q y r)))
            ((= x 1) (PX q y (add p r)))
            (t (PX q y (add (Pinj (- x 1) p) r))))))
   ((and (PXp l) (PXp r))
    (let ((p1 (PX-0 l))
          (x (PX-1 l))
          (p2 (PX-2 l))
          (q1 (PX-0 r))
          (y (PX-1 r))
          (q2 (PX-2 r)))
      (cond ((= x y) (PX (add p1 q1) x (add p2 q2)))
            ((< y x) (PX (add (PX p1 (- x y) 0) q1)
                         y
                         (add p2 q2)))
            (t (add r l)))))
   ((and (Pinjp l) (Pcp r))
    (add r l))
   ((and (PXp l) (Pcp r))
    (add r l))
   ((and (PXp l) (Pinjp r))
    (add r l))
   (t #|all ill-typed cases|# 0)))#|ACL2s-ToDo-Line|#
