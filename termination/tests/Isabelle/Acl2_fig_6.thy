(* Examples from ACL paper *)
theory Acl2_fig_6
  imports Main
begin

(* Fig 6 *)
function f6 :: "int \<Rightarrow> int"
and g :: "int \<Rightarrow> int"
and h :: "int \<Rightarrow> int" where
  "f6 x = (if x = 0 then 0 else
          (if x < 0 then g x else h x))"
| "g x = f6 (x+1)"
| "h x = f6 (x-1)"
by pat_completeness auto
termination by size_change

end