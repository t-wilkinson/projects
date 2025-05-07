{-# OPTIONS --without-K #-}
module NaturalNumbers where

open import Universes public

data 𝟙 : 𝓤₀ ̇  where
 ⋆ : 𝟙

data 𝟘 : 𝓤₀ ̇ where

variable
 𝓤 𝓥 𝓦 𝓣 : Universe

data ℕ : 𝓤₀ ̇ where
 zero : ℕ
 succ : ℕ → ℕ

ℕ-induction
 : {A : ℕ → 𝓤 ̇}
 → A zero
 → ((n : ℕ) → A n → A (succ n))
 → (n : ℕ)
 → A n
ℕ-induction a₀ f zero = a₀
ℕ-induction a₀ f (succ n) = f n (ℕ-induction a₀ f n)

ℕ-recursion
 : {X : 𝓤 ̇}
 → X
 → (ℕ → X → X)
 → ℕ
 → X
ℕ-recursion = ℕ-induction

ℕ-iteration
 : {X : 𝓤 ̇}
 → X
 → (X → X)
 → ℕ
 → X
ℕ-iteration x f = ℕ-recursion x (λ _ → f)

ℕ-induction'
 : (A : ℕ → 𝓤 ̇)
 → A zero
 → ((n : ℕ) → A n → A (succ n))
 → (n : ℕ)
 → A n
ℕ-induction' A a₀ f = h
 where
  h : (n : ℕ) → A n
  h zero = a₀
  h (succ n) = f n (h n)

ℕ-recursion'
 : (X : 𝓤 ̇)
 → X
 → (ℕ → X → X)
 → ℕ
 → X
ℕ-recursion' X = ℕ-induction' (λ _ → X)

ℕ-iteration'
 : (X : 𝓤 ̇)
 → X
 → (X → X)
 → ℕ
 → X
ℕ-iteration' X x f = ℕ-recursion' X x (λ _ x → f x)

module Arithmetic where
 _+_  _×_ : ℕ → ℕ → ℕ

 x + zero   = x
 x + succ y = succ (x + y)

 x × zero   = zero
 x × succ y = x + x × y

 infixl 20 _+_
 infixl 21 _×_

module Arithmetic' where
 _+_  _×_ : ℕ → ℕ → ℕ

--  x + y = ℕ-recursion x h y
--   where
--    h : ℕ → ℕ → ℕ
--    h x zero = x
--    h x (succ y) = y

 x + y = ℕ-iteration x succ y

 x × y = ℕ-iteration zero (x +_) y

 infixl 20 _+_
 infixl 21 _×_

module ℕ-order where
 _≤_ _≥_ : ℕ → ℕ → 𝓤₀ ̇

 zero ≤ y = 𝟙
 succ x ≤ zero = 𝟘
 succ x ≤ succ y = x ≤ y

 x ≥ y = x ≤ y

 infix 10 _≤_
 infix 10 _≥_

{-
module ℕ-order' where
 _≤_ _≥_ : ℕ → ℕ → 𝓤₀

 x ≤ y = (ℕ-induction {ℕ → ℕ → 𝓤 ̇} a₀ f x) y
  where
   a₀ : ℕ → 𝓤 ̇
   a₀ y = 𝟙
   f  (n : ℕ) → A n → A (succ n)
   f zero aₙ y = 𝟙
   f (succ x) aₙ zero = 𝟘
   f (succ x) aₙ (succ y) = f x aₙ y

 infix 10 _≤_
 infix 10 _≥_
 -}


{- TODO:
Exercise. After learning Σ and _＝_ explained below, prove that

x ≤ y if and only if Σ z ꞉ ℕ , x + z ＝ y.

Later, after learning univalence prove that in this case this implies

(x ≤ y) ＝ Σ z ꞉ ℕ , x + z ＝ y.
-}
