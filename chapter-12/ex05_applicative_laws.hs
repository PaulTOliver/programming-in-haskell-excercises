x :: Applicative a => a
pure id <*> x = x

g :: a -> b
x :: a
pure (g x) = pure g <*> pure x

x :: Applicative f => f (a -> b)
y :: a
x <*> pure y = pure (\g -> g y) <*> x

x :: Applicative f => f (b -> c)
y :: Applicative f => f (a -> b)
z :: Applicative f => f a
x <*> (y <*> z) = (pure (.) <*> x <*> y) <*> z
