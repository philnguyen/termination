(* Examples from the first-order size-change paper.
   I use conditionals instead of pattern matching to stay close to the paper *)
theory Sct_2
imports Main

begin

(* Ex 2: Original one can't be typed in an ML-like type system *)
(*
fun f :: "'a list \<Rightarrow> 'a \<Rightarrow> 'a"
and g :: "'a list \<Rightarrow> 'a \<Rightarrow> 'a list \<Rightarrow> 'a" where
  "f i x = (if i = [] then x else g(tl i, x, i))"
| "g a b c = f a (b # c)"
*)
datatype  tree  = L | N tree tree
(*
function f :: "tree \<Rightarrow> tree \<Rightarrow> tree" and
g :: "tree \<Rightarrow> tree \<Rightarrow> tree \<Rightarrow> tree" where
  "f L x = x"
| "f (N a b) x = g a x (N a b)"
| "g a b c = f a (N b c)"
  by pat_completeness auto
termination by size_change
*)                     
(*
function lft :: "tree \<Rightarrow> tree" where
"lft (N a b) = a"
| "lft (L) = L"
  by pat_completeness auto

function f2 :: "tree \<Rightarrow> tree \<Rightarrow> tree" and
g2 :: "tree \<Rightarrow> tree \<Rightarrow> tree \<Rightarrow> tree" where
  "f2 i x = (if i = L then x else g2 (lft i) x i)"
| "g2 a b c = f2 a (N b c)"
  by pat_completeness (simp, simp, simp)
termination by size_change
*)

end