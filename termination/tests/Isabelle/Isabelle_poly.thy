theory Isabelle_poly
  imports Main
begin

datatype poly = Pc nat | Pinj nat poly | PX poly nat poly
function add :: "poly \<Rightarrow> poly \<Rightarrow> poly" where
  "add (Pc a) (Pc b) = Pc (a+b)"
| "add (Pc c) (Pinj i P) = Pinj i (add P (Pc c))"
| "add (Pc c) (PX P i Q) = PX P i (add Q (Pc c))"
| "add (Pinj x P) (Pinj y Q) =
  (if x = y then Pinj x (add P Q) else
  (if y < x then Pinj y (add (Pinj (x-y) P) Q) else
   add (Pinj y Q) (Pinj x P)))"
| "add (Pinj x P) (PX Q y R) =
  (if x = 0 then add P (PX Q y R) else
  (if x = 1 then PX Q y (add P R) else
   PX Q y (add (Pinj (x-1) P) R)))"
| "add (PX P1 x P2) (PX Q1 y Q2) =
  (if x = y then PX (add P1 Q1) x (add P2 Q2) else
  (if y < x then PX (add (PX P1 (x-y) (Pc 0)) Q1) y (add P2 Q2)
   else add (PX Q1 y Q2) (PX P1 x P2)))"
| "add (Pinj i P) (Pc c) = add (Pc c) (Pinj i P)"
| "add (PX P i Q) (Pc c) = add (Pc c) (PX P i Q)"
| "add (PX Q y R) (Pinj x P) = add (Pinj x P) (PX Q y R)"
by pat_completeness auto
termination by size_change

end