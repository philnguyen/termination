theory Isabelle_foo
  imports Main
begin

function foo :: "bool \<Rightarrow> nat \<Rightarrow> nat \<Rightarrow> nat" where
  "foo True (Suc n) m = foo True n (Suc m)"
| "foo True 0 m = foo False 0 m"
| "foo False n (Suc m) = foo False (Suc n) m"
| "foo False n 0 = n"
by pat_completeness auto
termination by size_change

end