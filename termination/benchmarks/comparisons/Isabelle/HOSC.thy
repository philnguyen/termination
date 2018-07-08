(* Examples from the size-change for higher-order paper *)
theory HOSC
  imports Main
begin

(* Ex beginning of 1.2 *)
fun g :: "('a \<Rightarrow> 'a) \<Rightarrow> 'a \<Rightarrow> 'a" where
  "g r a = r (r a)"
fun f :: "nat \<Rightarrow> nat \<Rightarrow> nat" where
  "f n = (if n = 0 then Suc else g (f (n-1)))"

(* Ex later in 1.2 *)
fun foldr :: "('a \<Rightarrow> 'b \<Rightarrow> 'b) \<Rightarrow> 'b \<Rightarrow> 'a list \<Rightarrow> 'b" where
  "foldr h a xs = (if xs = [] then a else h (hd xs) (foldr h a (tl xs)))"
fun foldl :: "('b \<Rightarrow> 'a \<Rightarrow> 'b) \<Rightarrow> 'b \<Rightarrow> 'a list \<Rightarrow> 'b" where
  "foldl h a xs = (if xs = [] then a else foldl h (h a (hd xs)) (tl xs))"
fun reverse :: "'a list \<Rightarrow> 'a list" where                  
  "reverse xs = foldl (\<lambda> ys x. x # ys) [] xs"
fun append :: "'a list \<Rightarrow> 'a list \<Rightarrow> 'a list" where
  "append xs ys = foldr Cons xs ys"                    
fun concat :: "'a list list \<Rightarrow> 'a list" where
  "concat xss = foldr append [] xss"

(* Ex in 5.3: Can't type Y! *)
end                                                                          