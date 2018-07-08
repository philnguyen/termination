theory Vazou
  imports Main
begin

fun gcd :: "nat \<Rightarrow> nat \<Rightarrow> nat" where
  "gcd a 0 = a"
| "gcd a b = gcd b (a mod b)"

fun tfac :: "nat \<Rightarrow> nat \<Rightarrow> nat" where
  "tfac x n = (if n = 0 then x else tfac (n+x) (n-1))"

fun range :: "nat \<Rightarrow> nat \<Rightarrow> nat list" where
  "range lo hi = (if lo < hi then lo # (range (lo+1) hi) else [])"

fun map :: "('a \<Rightarrow> 'b) \<Rightarrow> 'a list \<Rightarrow> 'b list" where
  "map f (x # xs) = f x # (map f xs)"
| "map f [] = []"

fun merge :: "nat list \<Rightarrow> nat list \<Rightarrow> nat list" where
  "merge (x # xs) (y # ys) = (if x < y then x # (merge xs (y # ys)) else
                                            y # (merge (x # xs) ys))"
| "merge xs [] = xs"
| "merge [] ys = ys"
end