(* Examples from the size-change for higher-order paper *)
theory Ho_sct_fg
  imports Main
begin

(* Ex beginning of 1.2 *)
fun g :: "('a \<Rightarrow> 'a) \<Rightarrow> 'a \<Rightarrow> 'a" where
  "g r a = r (r a)"
fun f :: "nat \<Rightarrow> nat \<Rightarrow> nat" where
  "f n = (if n = 0 then Suc else g (f (n-1)))"

end