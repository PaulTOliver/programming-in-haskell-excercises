data List a = Nil | Cons a (List a)

len :: List a -> Int
len Nil = 0
len (Cons _ xs) = 1 + len xs

-- fun ways to declare List variables
l1 = Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))
l2 = 1 `Cons` (2 `Cons` (3 `Cons` Nil))
l3 = foldr Cons Nil [0..99]
