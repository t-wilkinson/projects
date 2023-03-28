-- |

module Optics.Lens where

{-
A Lens is a getter-setter
It lets us get at one part of a whole
When you have a field in a record, it creates a function which
takes the overarching data type and returns that fields type:

type Lens s t a b =
  forall (f :: * -> *). Functor f => (a -> f b) -> s -> f t
s structure *before* action
t structure *after* action
a focus *before* action
b focus *after* action

Laws ==>
- set l s (view l s) = s ::
  has no other effects
- view l (set l s a) = a ::
  have to be able to get something back out
- set l (set l s a) b = set l s b ::
  setting something twice should be eq to
  setting the last thing once

lens :: Functor f => (s -> a) (s -> b -> t) -> (a -> f b) -> s -> f t
over :: Lens s t a b -> (a -> b) -> s -> t
set :: Lens s t a b -> b -> s -> t
view' :: Lens' s a -> s -> a
-}

-- (^.) :: s -> Lens' s a -> a  -- flip view
-- (.~) :: Lens s t a b -> b -> s -> t  -- set
-- (%~) :: Lens s t a b -> (a -> b) -> s -> t  -- over
(&) = flip ($)
