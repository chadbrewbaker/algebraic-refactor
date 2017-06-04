import Data.Typeable
-- $ a^{mn} \rightarrow (a^{m})^{n}$
-- | 'curry' converts an uncurried function to a curried function.
curry                   :: ((a, b) -> c) -> a -> b -> c
curry f x y             =  f (x, y)


-- $(a^{m})^{n} \rightarrow a^{mn} $
-- | 'uncurry' converts a curried function to a function on pairs.
uncurry                 :: (a -> b -> c) -> ((a, b) -> c)
uncurry f p             =  f (fst p) (snd p)

--- Reference code from http://hackage.haskell.org/package/base-4.9.1.0/docs/src/Data.Tuple.html#curry


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

-- let k = m|n
--combineInput :: (m -> a, n -> a) -> (k -> a)
combineInput f = \t -> (fst f) t
combineInput f = \t -> (snd f) t 

--combineInput2 f = \t -> if typeOf t == typeOf  then else


data Foo = Bar | Baz
func1 Bar = 1
func2 Baz = 1

func = combineInput (func1, func2)
