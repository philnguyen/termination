(* Examples from the first-order size-change paper.
   I use conditionals instead of pattern matching to stay close to the paper *)
theory Sct_6
imports Main

begin

(* Ex 6 *)
fun f6 :: "'a list \<Rightarrow> 'a list \<Rightarrow> 'a list"
and g6 :: "'a list \<Rightarrow> 'a list \<Rightarrow> 'a list" where
  "f6 a b = (if b = [] then g6 a []
             else f6 ((hd b) # a) (tl b))"
| "g6 c d = (if c = [] then d
             else g6 (tl c) ((hd c) # d))"

end