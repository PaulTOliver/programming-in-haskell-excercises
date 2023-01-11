data Expr = Val Int | Add Expr Expr | Mult Expr Expr

data Op = EADD Expr | EMULT Expr | ADD Int | MULT Int
type Cont = [Op]

eval :: Expr -> Cont -> Int
eval (Val n) c = exec c n
eval (Add x y) c = eval x (EADD y : c)
eval (Mult x y) c = eval x (EMULT y : c)

exec :: Cont -> Int -> Int
exec [] n = n
exec (EADD y : c) n = eval y (ADD n : c)
exec (EMULT y : c) n = eval y (MULT n : c)
exec (ADD n : c) m = exec c (n + m)
exec (MULT n : c) m = exec c (n * m)

value :: Expr -> Int
value e = eval e []
