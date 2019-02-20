theory Lh_map
  imports Main
begin

fun map :: "('a \<Rightarrow> 'b) \<Rightarrow> 'a list \<Rightarrow> 'b list" where
  "map f (x # xs) = f x # (map f xs)"
| "map f [] = []"


end