theory Lh_range
  imports Main
begin

fun range :: "nat \<Rightarrow> nat \<Rightarrow> nat list" where
  "range lo hi = (if lo < hi then lo # (range (lo+1) hi) else [])"

end