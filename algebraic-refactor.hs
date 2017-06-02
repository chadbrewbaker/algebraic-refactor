-- | 'curry' converts an uncurried function to a curried function.
curry                   :: ((a, b) -> c) -> a -> b -> c
curry f x y             =  f (x, y)

-- | 'uncurry' converts a curried function to a function on pairs.
uncurry                 :: (a -> b -> c) -> ((a, b) -> c)
uncurry f p             =  f (fst p) (snd p)

--- Reference code from http://hackage.haskell.org/package/base-4.9.1.0/docs/src/Data.Tuple.html#curry
