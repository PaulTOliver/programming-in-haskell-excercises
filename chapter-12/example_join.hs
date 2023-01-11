join :: Monad m => m (m a) -> m a
join mmx = do
	mx <- mmx
	x <- mx
	return x
