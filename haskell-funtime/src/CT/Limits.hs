module CT.Limits where

{-
[-,-] : Cop × C → C
-}

-- Cones → CoCones
data Op e a = Op (a → e)

-- Cones → Cones
data Reader e a = Reader (e → a)
