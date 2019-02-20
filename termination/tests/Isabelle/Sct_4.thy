(* Examples from the first-order size-change paper.
   I use conditionals instead of pattern matching to stay close to the paper *)
theory Sct_4
imports Main

begin

(* Ex 4 *)
function p :: "nat \<Rightarrow> nat \<Rightarrow> nat \<Rightarrow> nat" where
  "p m n r = (if r > 0 then p m (r-1) n else
             (if n > 0 then p r (n-1) m else m))"
by pat_completeness auto
termination by size_change

end