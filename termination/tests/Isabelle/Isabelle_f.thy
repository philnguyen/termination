theory Isabelle_f
  imports Main
begin

(* Section 3 *)
function f :: "nat \<Rightarrow> nat \<Rightarrow> nat" where
  "f n 0 = n"
| "f 0 (Suc m) = f (Suc m) (Suc m)"
| "f (Suc n) (Suc m) = f m n"
by pat_completeness auto
termination by size_change

end