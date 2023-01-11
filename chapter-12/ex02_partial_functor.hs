-- The below definition illustrates how we can visualize a functional type as
-- a parametrized data type (named `(->)`). The arrow operator acts as a name
-- for a parametrized data type (that's just mind blowing!!!).
--
-- We can therefore turn these partial-function types into different class
-- instances.

f1 :: ((->) a) b
f1 = undefined

-- I comment the answer out as it clashes with definitions on the STD prelude.
-- This shows that both `fmap` and the composition operator `(.)` can be used
-- interchangeably to compose functions. Fascinating stuff indeed.

--instance Functor ((->) a) where
--	-- fmap :: (b -> c) -> (a -> b) -> (a -> c)
--	fmap g f = g . f

v1 = (+5) . (+6) $ 1 -- = 12
v2 = fmap (+5) (+6) $ 1 -- = 12
v3 = (+5) <$> (+6) $ 1 -- = 12
