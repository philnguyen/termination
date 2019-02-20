theory Lh_gcd
  imports Main
begin

fun gcd :: "nat \<Rightarrow> nat \<Rightarrow> nat" where
  "gcd a 0 = a"
| "gcd a b = gcd b (a mod b)"

end