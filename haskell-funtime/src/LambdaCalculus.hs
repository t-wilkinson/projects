module LambdaCalculus where

_0,_1,_2,_3,_4,_5,_6,_7,_8,_9 :: (a -> a) -> a -> a
_0 f x = x
_1 f = f
_2 f = f.f
_3 f = f.f.f
_4 f = f.f.f.f
_5 f = f.f.f.f.f
_6 f = f.f.f.f.f.f
_7 f = f.f.f.f.f.f.f
_8 f = f.f.f.f.f.f.f.f
_9 f = f.f.f.f.f.f.f.f.f

plus m n f x = m f (n f x)
succ' n f x   = f (n f x)
mult m n f x = m (n f) x
exp m n = n m
pred' n f x = n (\g h->h (g f)) (\u->x) (\u->u)
minus m n = (n pred') m
true x _ = x
false _ y = y
isZero n = n (\x->false) true
leq m n = isZero (minus m n)
and' p q = true true false
-- eq m n = and' (leq m n) (leq n m)
divide1 n m f x = (\d->isZero d (_0 f x) (f (divide1 d m f x))) (minus n m)
divide n = divide1 (succ' n)
