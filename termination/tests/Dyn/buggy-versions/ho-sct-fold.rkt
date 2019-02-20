#lang racket
(require termination)

(define (foldr op a xs) (if (null? xs) a (op (car xs) (foldr op a xs))))
(define (foldl op a xs) (if (null? xs) a (foldl op (op (car xs) a) xs)))
(define (reverse xs) (foldl (λ (ys x) (cons x ys)) '() xs))
(define (@ xs ys) (foldr cons xs ys))
(define (concat xss) (foldr @ '() xss))

(require rackunit)
(check-exn exn? (λ () (begin/termination (reverse '(1 2 3)))))
(check-exn exn? (λ () (begin/termination (@ '(1 2 3) '(4 5 6)))))
(check-exn exn? (λ () (begin/termination (concat '((1 2 3) (4 5 6) (7 8 9))))))

