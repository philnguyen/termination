(* Examples from the first-order size-change paper.
   I use conditionals instead of pattern matching to stay close to the paper *)
theory Sct_5
imports Main

begin

(* Ex 5 *)
function f :: "'a list \<Rightarrow> 'a list \<Rightarrow> 'a list" where
  "f x y = (if y = [] then x else
           (if x = [] then f y (tl y) else
                           f y (tl x)))"
by pat_completeness auto
termination by size_change

end