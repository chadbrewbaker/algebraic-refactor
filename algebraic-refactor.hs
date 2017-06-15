-- A Haskell implementation of Jr. High Algebra equalties
-- Motivation is creating paths so you can do type homotopy searches

-- $ a^{mn} \rightarrow (a^{m})^{n}$
-- | 'curry' converts an uncurried function to a curried function.
curry                   :: ((a, b) -> c) -> a -> b -> c
curry f x y             =  f (x, y)

-- $(a^{m})^{n} \rightarrow a^{mn} $
-- | 'uncurry' converts a curried function to a function on pairs.
uncurry                 :: (a -> b -> c) -> ((a, b) -> c)
uncurry f p             =  f (fst p) (snd p)

---- curry and uncurry are from http://hackage.haskell.org/package/base-4.9.1.0/docs/src/Data.Tuple.html#curry


-- $ (ab)^c \rightarrow a^c b^c$
-- | 'separate' splits a unary function with an output tuple into separate functions
separate :: (t -> (a, b)) -> (t -> a, t -> b)
separate f = (\t -> fst (f t), \t -> snd (f t))


-- $ a^c b^c \rightarrow  (ab)^c$ 
-- | 'combine' takes two functions on the same output and concatinates the outputs
combine :: (t -> a, t -> b) -> t -> (a, b)
combine f = (\t -> ((fst f) t, (snd f) t))
                where
                g = fst f
                h = snd f

-- $a^m a^n \rightarrow a^{m+n}$  
--uncocurry :: ((a -> c), (b -> c)) -> (Either a b) ->( Either a b -> c)
uncocurry f x = either (fst f) (snd f) x

-- $a^{m+n} \rightarrow a^m a^n$
--cocurry :: (Either a b -> c) -> (a -> c, b -> c)
cocurry f = \x -> (f (Left x), f (Right x))


-- Let m = n+k
-- a^{m}\div a^{n} \rightarrow a^{m-n}
-- m = Either n k
-- domainShrink :: ((Either n k) -> a) -> (k -> a)  
-- domainShrink f x  = f (Right x)  


domainShrink :: ((n-> a), (k -> a)) -> (k -> a)  
domainShrink (f,g) = undefined


