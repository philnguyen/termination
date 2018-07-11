(* Examples from the first-order size-change paper.
   I use conditionals instead of pattern matching to stay close to the paper *)
theory FOSC
imports Main

begin

(* Ex 1 *)
fun r1 :: "'a list \<Rightarrow> 'a list \<Rightarrow> 'a list" where
  "r1 ls a = (if ls = [] then a else r1 (tl ls) (hd ls # a))"
fun rev :: "'a list \<Rightarrow> 'a list" where
  "rev ls = r1 ls []"

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


(* Ex 3 *)
fun ack :: "nat \<Rightarrow> nat \<Rightarrow> nat" where
  "ack m n = (if m = 0 then n + 1 else
             (if n = 0 then ack (m-1) 1 else
                            ack (m-1) (ack m (n-1))))"

(* Ex 4 *)
function p :: "nat \<Rightarrow> nat \<Rightarrow> nat \<Rightarrow> nat" where
  "p m n r = (if r > 0 then p m (r-1) n else
             (if n > 0 then p r (n-1) m else m))"
by pat_completeness auto
termination by size_change

(* Ex 5 *)
function f :: "'a list \<Rightarrow> 'a list \<Rightarrow> 'a list" where
  "f x y = (if y = [] then x else
           (if x = [] then f y (tl y) else
                           f y (tl x)))"
by pat_completeness auto
termination by size_change

(* Ex 6 *)
fun f6 :: "'a list \<Rightarrow> 'a list \<Rightarrow> 'a list"
and g6 :: "'a list \<Rightarrow> 'a list \<Rightarrow> 'a list" where
  "f6 a b = (if b = [] then g6 a []
             else f6 ((hd b) # a) (tl b))"
| "g6 c d = (if c = [] then d
             else g6 (tl c) ((hd c) # d))"

end