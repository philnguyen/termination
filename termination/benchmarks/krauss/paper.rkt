#lang racket/base

(require racket/match
         "../../main.rkt")

(let () ; Section 3 intro
  (define f
    (terminating-function/c
     (match-lambda**
      [(n               0              ) n]
      [(0               (? positive? m)) (f m m)]
      [((? positive? n) (? positive? m)) (f (sub1 m) (sub1 n))])))
  (f 10 20))

(let () ; Section 5
  
  (define/termination (p m n r)
    (cond [(< 0 r) (p m (- r 1) n)]
          [(< 0 n) (p r (- n 1) m)]
          [else m]))
  (p 7 8 9)

  (define foo 
    (terminating-function/c
     (match-lambda**
      [(#t (? positive? n) m              ) (foo #t (sub1 n) (add1 m))]
      [(#t 0               m              ) (foo #f 0        m       )]
      [(#f n               (? positive? m)) (foo #f (add1 n) (sub1 m))]
      [(#f n               0              ) n])))
  (with-custom-< (λ (x y)
                   (cond [(and (integer? x) (integer? y)) (< -1 x y)]
                         [else (and (not x) y)]))
    (foo #t 4 5))

  ;; This works *maybe* because of luck:
  ;; the implementation spares the first iteration of every loop to detect entry
  (define bar
    (terminating-function/c
     (match-lambda**
      [(0               (? positive? n) m) (bar m m m)]
      [((? positive? v) n               m) 0]
      [(k               0               m) 0])))
  (bar 3 4 5)

  
  (struct Pc (c) #:transparent)
  (struct Pinj (i P) #:transparent)
  (struct PX (P i Q) #:transparent)
  (define mkPX #|is it?|# PX)
  (define mkPinj #|is it?|# Pinj)
  (define add
    (terminating-function/c
     (match-lambda**
      [((Pc a) (Pc b)) (Pc (+ a b))]
      [((Pc c) (Pinj i P)) (Pinj i (add P (Pc c)))]
      [((Pc c) (PX P i Q)) (PX P i (add Q (Pc c)))]
      [((Pinj x P) (Pinj y Q))
       (cond [(= x y) (mkPinj x (add P Q))]
             [(< y x) (mkPinj y (add (Pinj (- x y) P) Q))]
             [else #|permute|# (add (Pinj y Q) (Pinj x P))])]
      [((Pinj x P) (PX Q y R))
       (case x
         [(0)  (add P (PX Q y R))]
         [(1)  (PX Q y (add P R))]
         [else (PX Q y (add (Pinj (- x 1) P) R))])]
      [((PX P₁ x P₂) (PX Q₁ y Q₂))
       (cond [(= x y) (mkPX (add P₁ Q₁) x (add P₂ Q₂))]
             [(< y x) (mkPX (add (PX P₁ (- x y) (Pc 0)) Q₁)
                            y
                            (add P₂ Q₂))]
             [else #|permute|# (add (PX Q₁ y Q₂) (PX P₁ x P₂))])]
      [((Pinj i P) (Pc c)) #|permute|# (add (Pc c) (Pinj i P))]
      [((PX P i Q) (Pc c)) #|permute|# (add (Pc c) (PX P i Q))]
      [((PX Q y R) (Pinj x P)) #|permute|# (add (Pinj x P) (PX Q y R))])))
  (with-custom-< (letrec ([≺ (match-lambda**
                              [((Pc _) (not (? Pc?))) #t]
                              [((Pc c) (Pc d)) (< c d)]
                              [((Pinj _ _) (PX _ _ _)) #t]
                              [((Pinj x P) (Pinj y Q)) (or (< x y) (≺ P Q))]
                              [((PX P₁ x Q₁) (PX P₂ y Q₂))
                               (or (< x y) (≺ P₁ P₂) (≺ Q₁ Q₂))]
                              [(_ _) #f])])
                   ≺)
    (add (Pinj 42 (Pc 0)) (Pc 42))))
