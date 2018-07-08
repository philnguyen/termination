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
        (t 0)))#|ACL2s-ToDo-Line|#


;; TODO: `add` needs user-defined data