#lang racket
(require termination)

(define (foldr op a xs) (if (null? xs) a (op (car xs) (foldr op a (cdr xs)))))
(define (foldl op a xs) (if (null? xs) a (foldl op (op (car xs) a) (cdr xs))))
(define (reverse xs) (foldl (λ (ys x) (cons x ys)) '() xs))
(define (@ xs ys) (foldr cons xs ys))
(define (concat xss) (foldr @ '() xss))
(begin/termination (reverse '(1 2 3)))
(begin/termination (@ '(1 2 3) '(4 5 6)))
(begin/termination (concat '((1 2 3) (4 5 6) (7 8 9))))
