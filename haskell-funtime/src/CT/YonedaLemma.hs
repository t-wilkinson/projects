{-# Language
    RankNTypes
#-}

{-|
Conceptualize the Yoneda Lemma

The natural transformations each correspond to an element of C

-}

module CT.YonedaLemma where


check :: Functor f => f a -> (∀ b . (a -> b) -> f b)
check x f = fmap f x

uncheck :: Functor f => (∀ b . (a -> b) -> f b) -> f a
uncheck f = f id

data Color = Red | Green | Blue        deriving (Show, Eq)
data Note  = C | D | E | F | G | A | B deriving (Show, Eq)

colorMap x = if x then Blue else Red
heatMap x = if x then 32 else 212
soundMap x = if x then C else G
idBool :: Bool -> Bool
idBool x = x

-- main = print $ check xs idBool


--test :: f a -> f a
--test = uncheck . check

xs = [True, False, True, False]
test = check xs (colorMap . idBool) == fmap colorMap xs



imager :: ∀ r . ((Bool -> r) -> [r])
imager iffie = fmap iffie [True, False, True, True]

fw :: (Functor f) => (∀ b . (a -> b) -> f b) -> f a
fw f = f id

bw :: (Functor f) => f a -> (∀ b . (a -> b) -> f b)
bw x f = fmap f x
