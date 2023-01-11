-- After finding out functors and applicatives for the partially-applied
-- function type, the monad implementation is easy.

--instance Monad ((->) a) where
--	-- return :: b -> (a -> b)
--	return = const
--
--	-- (>>=) :: (b -> a) -> (a -> b -> c) -> (b -> c)
--	x (>>=) y = \r -> y (x r) r

-- We can use this to combine functions that take 1 argument with functions
-- that take two arguments. We process the passed value using the first
-- function and use that as a first argument to the second function. We use
-- the raw passed value as the second argument to the second function.
v1 = (+5) >>= (+) $ 3 -- ((3 + 5) + 3) == 11
