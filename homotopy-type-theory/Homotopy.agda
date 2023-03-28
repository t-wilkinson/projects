{-# OPTIONS --without-K #-}
module Homotopy where

open import Universes public
open import HoTT public

data S₁ : 𝓤₀ ̇ where
 base : S₁
 loop : base == base → S₁

double-loop : S₁
double-loop = loop (id-transitivity (refl base) (refl base))

reverse-S₁ : S₁ → S₁
reverse-S₁ (base) = base
reverse-S₁ (loop (refl x)) = loop (reverse-id (refl x))

homotopy : double-loop == loop (refl base)
homotopy = refl (loop (refl base))

-- data S₂ : 𝓤₀ ̇ where
--  base₂ : S₂
--  surface : Id S₁ (loop (refl base base)) (loop (refl base base)) → S₂
