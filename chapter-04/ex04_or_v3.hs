(||) :: Bool -> Bool -> Bool
False || b = b
True || _ = True
