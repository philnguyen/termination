(* Examples from ACL paper *)
theory Acl2_fig_7
  imports Main
begin

(* Fig 7 *)
function f7 :: "int \<Rightarrow> int" where
  "f7 x = (if x \<le> 1 then 0 else
          (if x mod 2 = 1 then f7 (x+1)
           else 1 + f (x div 2)))"
by pat_completeness auto
termination by size_change

end