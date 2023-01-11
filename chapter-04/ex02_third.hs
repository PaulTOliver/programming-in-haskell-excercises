third_v1 xs = head (tail (tail xs))
third_v2 xs = xs !! 2
third_v3 (_:_:x:_) = x
