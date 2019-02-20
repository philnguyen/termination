theory Isabelle_bar
  imports Main
begin

function bar :: "nat \<Rightarrow> nat \<Rightarrow> nat \<Rightarrow> nat" where
  "bar 0 (Suc n) m = bar m m m"
| "bar (Suc v) n m = 0"
| "bar k 0 m = 0"
  by pat_completeness auto
termination by size_change

end