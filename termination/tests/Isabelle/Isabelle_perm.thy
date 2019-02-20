theory Isabelle_perm
  imports Main
begin

(* Section 5 *)
function p :: "nat \<Rightarrow> nat \<Rightarrow> nat \<Rightarrow> nat" where
  "p m n r = (if 0 < r then p m (r-1) n else
             (if 0 < n then p r (n-1) m else m))"
by pat_completeness auto
termination by size_change

end