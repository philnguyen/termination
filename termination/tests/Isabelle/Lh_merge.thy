theory Lh_merge
  imports Main
begin

fun merge :: "nat list \<Rightarrow> nat list \<Rightarrow> nat list" where
  "merge (x # xs) (y # ys) = (if x < y then x # (merge xs (y # ys)) else
                                            y # (merge (x # xs) ys))"
| "merge xs [] = xs"
| "merge [] ys = ys"
end