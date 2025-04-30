{-# Language
  MultiParamTypeClasses
, TupleSections
#-}

module CT.Adjunction where

-- f :: producer
    -- produces exactly what 'g' needs
    -- the optimal answer to the question posed by 'g'
-- g :: consumer
    -- the most difficult question the the 'f' can anser

class (Functor f, Functor g) => Adjunction f g where
    leftAdjunct :: (f a -> b) -> a -> g b
    leftAdjunct h = fmap h . unit

    rightAdjunct :: (a -> g b) -> f a -> b
    rightAdjunct h = counit . fmap h

    unit :: a -> g (f a)
    unit = leftAdjunct id

    counit :: f (g a) -> a
    counit = rightAdjunct id

-- instance Adjunction (,s) (s->) where
--         leftAdjunct = curry
--         rightAdjunct = uncurry

type Store s a = (s -> a,s)
type State s a = s -> (a,s)


-- describe (examples d) <= d
-- e (= examples (describe e)

-- describe e <= d  <=> examples d (= e
