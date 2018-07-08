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
;; Fig 2
(defun f (x)
  (cond ((or (not (integerp x)) (= x 0)) 0)
        ((< x 0) (f (+ x 1)))
        (t (f (- x 1)))))
(defun dec (x)
  (if (or (not (integerp x)) (<= x 0))
    255
    (- x 1)))
#|(defun foo (i j)
  (cond ((not (and (natp i) (natp j))) 0)
        ((= i 1) (if (= j 1) 0 (foo (dec j) (dec j))))
        (t (foo (dec i) j))))
|#
;; Fig 6
#|
(mutual-recursion
 (defun g (x) (if (integerp x) (f6 (+ x 1)) 0))
 (defun h (x) (if (integerp x) (f6 (- x 1)) 0))
 (defun f6 (x)
   (cond ((or (not (integerp x)) (= x 0)) 0)
         ((< x 0) (g x))
         (t (h x)))))
|#

;; Fig 7
(defun f7 (x)
  (cond ((or (not (integerp x)) (<= x 1)) 0)
        ((= (mod x 2) 1) (f (+ x 1)))
        (t (+ 1 (f (nonnegative-integer-quotient x 2))))))#|ACL2s-ToDo-Line|#
