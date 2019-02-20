theory Lh_tfact
  imports Main
begin

fun tfac :: "nat \<Rightarrow> nat \<Rightarrow> nat" where
  "tfac x n = (if n = 0 then x else tfac (n+x) (n-1))"

end