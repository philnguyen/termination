(* Examples from ACL paper *)
theory Acl2_fig_2
  imports Main
begin

(* Fig 2 *)
fun f :: "int \<Rightarrow> int" where
  "f x = (if x = 0 then 0 else
         (if x < 0 then f (x+1) else
          f (x-1)))"
fun dec :: "int \<Rightarrow> int" where
  "dec x = (if x \<le> 0 then 255 else x-1)"
function foo :: "int \<Rightarrow> int \<Rightarrow> int" where
  "foo i j = (if i = 1 then
                 (if j = 1 then 0 else foo (dec j) (dec j))
              else foo (dec i) j)"
  by pat_completeness auto
termination by size_change

end