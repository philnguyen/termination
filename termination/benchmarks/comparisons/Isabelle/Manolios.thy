(* Examples from ACL paper *)
theory Manolios
  imports Main
begin

(* Fig 2 *)
fun f :: "int \<Rightarrow> int" where
  "f x = (if x = 0 then 0 else
         (if x < 0 then f (x+1) else
          f (x-1)))"
fun dec :: "int \<Rightarrow> int" where
  "dec x = (if x \<le> 0 then 255 else x-1)"
fun foo :: "int \<Rightarrow> int \<Rightarrow> int" where
  "foo i j = (if i = 1 then
                 (if j = 1 then 0 else foo (dec j) (dec j))
              else foo (dec i) j)"

(* Fig 6 *)
fun f6 :: "int \<Rightarrow> int"
and g :: "int \<Rightarrow> int"
and h :: "int \<Rightarrow> int" where
  "f6 x = (if x = 0 then 0 else
          (if x < 0 then g x else h x))"
| "g x = f6 (x+1)"
| "h x = f6 (x-1)"

(* Fig 7 *)
fun f7 :: "int \<Rightarrow> int" where
  "f7 x = (if x \<le> 1 then 0 else
          (if x mod 2 = 1 then f7 (x+1)
           else 1 + f (x div 2)))"

end