#lang racket/base

(require racket/match
         racket/function
         racket/list
         racket/splicing
         racket/bool
         rackunit
         "main.rkt")

(let () ; decreasing on `y` can't fool the checker
  (define (f x y)
    (if (zero? x)
        42
        (f (sub1 y) (add1 x))))
  
  (check-exn exn? (λ () (begin/termination (f 3 3)))))

(let () ; factorial ok on naturals, diverges and killed on negatives
  (define/termination (fact n) (if (zero? n) 1 (* n (fact (sub1 n)))))
  (fact 10)
  (check-exn exn? (λ () (fact -1))))

(let ()
  (define (quicksort l)
    (let go ([l l])
      (match l
        [(cons x xs)
         (define-values (<xs ≥xs) (partition (curry < x) xs))
         (append (go <xs) (list x) (go ≥xs))]
        ['() '()])))
  (define (quicksort:incorrect l)
    (let go ([l l])
      (match l
        [(cons x xs)
         (define-values (<xs ≥xs) (partition (curry < x) xs))
         (append (go <xs) (go (cons x ≥xs)))]
        ['() '()])))
  (begin/termination (quicksort '(1 1 1 1 1)))
  (check-exn exn? (λ () (begin/termination (quicksort:incorrect '(1 1 1 1 1))))))

(let () ; Static analysis has to conflate many `(λ (n) …)` closures
  (define (length/cps xs k)
    (if (null? xs)
        (k 0)
        (length/cps (cdr xs) (λ (n) (add1 (k n))))))
  (define (length xs) (length/cps xs values))
  (begin/termination (length '(a b c d e))))

(let ()
  ;; e ::= (λ (x) e) | x | (e e)
  ;; v ::= (λ (x) e) × ρ
  ;; ρ = x → v
  ;; ev : e ρ → v 
  (define (ev e [ρ (hasheq)])
    (match e
      [`(λ (,x) ,_)  (cons e ρ)]
      [(? symbol? x) (hash-ref ρ x (λ () (error 'ev "no ~a" x)))]
      [`(,e₁ ,e₂)
       (match* ((ev e₁ ρ) (ev e₂ ρ))
         [((cons `(λ (,x) ,e*) ρ*) v) (ev e* (hash-set ρ* x v))]
         [(v₁ _) (error 'ev "~a evals to ~a, not function" e₁ v₁)])]))

  (define (-let x eₓ e) `((λ (,x) ,e) ,eₓ))
  (define -Z `(λ (f) (λ (x) x)))
  (define -S `(λ (n) (λ (f) (λ (x) (f ((n f) x))))))
  (define -plus `(λ (m) (λ (n) (λ (f) (λ (x) ((m f) ((n f) x)))))))
  (define -one `(,-S ,-Z))
  (define -two `((,-plus ,-one) ,-one)) 

  ;; ≺ : (U e ρ) (U e ρ) → Boolean
  (splicing-local
      ;; The custom order doesn't need to escape instrumentation,
      ;; But instrumenting this tight loop significantly slows down this supposedly quick test
      ((define-syntax-rule (#%app f x ...) (#%plain-app f x ...)))
    ;; Check if `e₁`'s node count is strictly smaller than `e₂`'s
    (define (e≺ e₁ e₂)
      (define size
        (match-lambda
          [`(λ ,_ ,e) (+ 1 (size e))]
          [(? symbol? x) 1]
          [`(,e₁ ,e₂) (+ (size e₁) (size e₂))]))
      (< (size e₁) (size e₂)))
    ;; Check if `ρ₁` is strictly a sub-structure of `ρ₂`
    (define (ρ≺ ρ₁ ρ₂)
      (for/or ([v (in-hash-values ρ₂)])
        (define ρ* (cdr v))
        (or (equal? ρ₁ ρ*) (ρ≺ ρ₁ ρ*))))
    (define (≺ x y)
      (or (and (hash? x) (hash? y)             (ρ≺ x y))            
          (and (not (hash? x)) (not (hash? y)) (e≺ x y)))))

  (with-custom-< ≺
    (begin/termination (ev '((λ (x) (x x)) (λ (y) y))))
    (begin/termination (ev -two))
    (check-exn exn? (λ () (begin/termination (ev '((λ (x) (x x)) (λ (y) (y y))))))))
  
  (check-exn exn? (λ () (begin/termination (ev '((λ (x) (x x)) (λ (y) (y y)))))))
  ;; below only works with `apply-with-specialized-termination`
  #;(begin/termination (ev '((λ (x) (x x)) (λ (y) y))))
  )

(let ()
    (define (sort-list obj pred)

      (define (loop l)
        (if (and (pair? l) (pair? (cdr l)))
            (split l '() '())
            l))

      (define (split l one two)
        (if (pair? l)
            (split (cdr l) two (cons (car l) one))
            (merge (loop one) (loop two))))

      (define (merge one two)
        (cond ((null? one) two)
              ((pred (car two) (car one))
               (cons (car two)
                     (merge (cdr two) one)))
              (else
               (cons (car one)
                     (merge (cdr one) two)))))

      (loop obj))

    (with-custom-< (λ (l r) (and (list? l) (list? r) (< (length l) (length r))))
      (begin/termination
        (sort-list '("one" "two" "three" "four" "five" "six"
                           "seven" "eight" "nine" "ten" "eleven" "twelve")
                   string<?))))

(let ()
  (define (f x)
    (define g (cond [(> x 0) (compose f sub1)]
                    [(< x 0) (compose f add1)]
                    [else values]))
    (g x))
  (begin/termination (f 3)))


;; Tests revealing errors in initial implementation pointed out by Amir ben-Amram
(let ()
  (define/termination (f x y z)
    (match* (x y z)
      [(1 2 3) (f 1 2 0)]
      [(1 2 0) (f 2 1 0)]
      [(2 1 0) (f 1 2 0)]))
  (check-exn exn? (λ () (f 1 2 3)))
  (define/termination (g x y) (g y x))
  (check-exn exn? (λ () (g 1 2))))

;; This is a terminating program, but not by SCT without further annotations
(let ()
  ; graph as an adjacency list
  (define g1 '((a b 1) (a c 3) (b c 1) (b d 5) (c d 2)))

  (define/termination (list-union l1 l2)
    (remove-duplicates (append l1 l2)))

  ; possibly broken on self-edges
  (define/termination (vertices-of-edge e)
    (list (car e) (cadr e)))

  (define/termination (vertices E)
    (foldr list-union '() (map vertices-of-edge E)))

  (define/termination (edge-from? v e)
    (or (equal? v (car e))
        (equal? v (cadr e))))

  (define/termination (edges-from v E)
    (filter (edge-from? v) E))

  ; trees are just lists holding vertices and an adjacency list
  ; INVARIANT: edges should all be in the vertex list
  ; we need this slightly funny representation to account for trees with just one vertex (our starting case)
  (define/termination (tree-vertices T) (car T))
  (define/termination (tree-edges T) (cadr T))

  (define/termination (in-tree? v T)
    (member v (tree-vertices T)))

  (define/termination ((edge-out-of-tree? T) e)
    (xor (in-tree? (car e) T)
         (in-tree? (cadr e) T)))

  (define/termination (add-edge-to-tree e T)
    (list (list-union (vertices-of-edge e) (tree-vertices T))
          (cons e (tree-edges T))))

  (define/termination (prims-loop T E)
    (let ([outbound (filter (edge-out-of-tree? T) E)])
      (printf "prims-loop\n\t~a\n\t~a\n\n" T outbound)
      (if (null? outbound)
          T
          (let ([best (argmin caddr outbound)])
            (prims-loop (add-edge-to-tree best T) E)))))

  (define/termination (prims E)
    (define init (car (shuffle (vertices E))))
    (prims-loop `((,init) ()) E))

  (check-exn exn? (λ () (prims g1))))

;; This program is confirmed terminating thanks to transformed argument
(let ()
  ; graph as an adjacency list
  (define g1 '((a b 1) (a c 3) (b c 1) (b d 5) (c d 2)))

  (define/termination (list-union l1 l2)
    (remove-duplicates (append l1 l2)))

  ; possibly broken on self-edges
  (define/termination (vertices-of-edge e)
    (list (car e) (cadr e)))

  (define/termination (vertices E)
    (foldr list-union '() (map vertices-of-edge E)))

  (define/termination (edge-from? v e)
    (or (equal? v (car e))
        (equal? v (cadr e))))

  (define/termination (edges-from v E)
    (filter (edge-from? v) E))

  ; trees are just lists holding vertices and an adjacency list
  ; INVARIANT: edges should all be in the vertex list
  ; we need this slightly funny representation to account for trees with just one vertex (our starting case)
  (define/termination (tree-vertices T) (car T))
  (define/termination (tree-edges T) (cadr T))

  (define/termination (in-tree? v T)
    (member v (tree-vertices T)))

  (define/termination ((edge-out-of-tree? T) e)
    (xor (in-tree? (car e) T)
         (in-tree? (cadr e) T)))

  (define/termination (add-edge-to-tree e T)
    (list (list-union (vertices-of-edge e) (tree-vertices T))
          (cons e (tree-edges T))))

  (define/termination (prims-loop T E)
    (let ([outbound (filter (edge-out-of-tree? T) E)])
      (printf "prims-loop\n\t~a\n\t~a\n\n" T outbound)
      (if (null? outbound)
          T
          (let ([best (argmin caddr outbound)])
            (prims-loop (add-edge-to-tree best T) E)))))

  (define/termination (prims E)
    (define init (car (shuffle (vertices E))))
    (prims-loop `((,init) ()) E))

  (parameterize ([argument-transformer
                  ;; Ignore the actual arguments and count remaining edges in pool
                  (match-lambda
                    [(list (list _ T) E) (list (- (length E) (length T)))])])
    (prims g1)))
