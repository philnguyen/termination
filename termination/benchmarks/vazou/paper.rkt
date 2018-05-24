#lang racket/base

(require racket/match
         "../../main.rkt")

(let () ; section 5.1
  (define gcd
    (terminating-function/c
     (match-lambda**
      [(a 0) a]
      [(a b) (gcd b (modulo a b))])))
  (gcd 102 7)

  (define tfact
    (terminating-function/c
     (match-lambda**
      [(x 0) x]
      [(x n) (tfact (* n x) (- n 1))])))
  (tfact 5 1)

  (define/termination (range lo hi)
    ;; ugly cheat? but their annotation counts as custom ordering too
    (with-custom-< (λ (x y)
                     (and (integer? x) (integer? y)
                          (< (- hi x) (- hi y))))
      (cond [(< lo hi) (cons lo (range (add1 lo) hi))]
            [else '()])))
  (range 2 6)

  (define/termination (map f xs)
    (match xs
      [(cons x xs) (cons (f x) (map f xs))]
      [_ '()]))
  (map add1 '(1 2 4))

  (define merge
    (terminating-function/c
     (match-lambda**
      [((cons x xs) (cons y ys))
       (cond [(< x y) (cons x (merge xs (cons y ys)))]
             [else (cons y (merge (cons x xs) ys))])]
      [('() ys) ys]
      [(xs _) xs])))

  (merge '(2 4 6) '(3 5))
  )
