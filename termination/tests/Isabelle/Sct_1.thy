(* Examples from the first-order size-change paper.
   I use conditionals instead of pattern matching to stay close to the paper *)
theory Sct_1
imports Main

begin

(* Ex 1 *)
fun r1 :: "'a list \<Rightarrow> 'a list \<Rightarrow> 'a list" where
  "r1 ls a = (if ls = [] then a else r1 (tl ls) (hd ls # a))"
fun rev :: "'a list \<Rightarrow> 'a list" where
  "rev ls = r1 ls []"

end