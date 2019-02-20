(* Examples from the first-order size-change paper.
   I use conditionals instead of pattern matching to stay close to the paper *)
theory Sct_3
imports Main

begin

(* Ex 3 *)
fun ack :: "nat \<Rightarrow> nat \<Rightarrow> nat" where
  "ack m n = (if m = 0 then n + 1 else
             (if n = 0 then ack (m-1) 1 else
                            ack (m-1) (ack m (n-1))))"

end