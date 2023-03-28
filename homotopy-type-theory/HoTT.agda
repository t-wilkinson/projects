{-# OPTIONS --without-K #-}
module HoTT where

open import Universes public

open import Agda.Builtin.IO using (IO)
open import Agda.Builtin.Unit using (⊤)
open import Agda.Builtin.String using (String)

postulate putStrLn : String → IO ⊤
{-# FOREIGN GHC import qualified Data.Text as T #-}
{-# COMPILE GHC putStrLn = putStrLn . T.unpack #-}

main : IO ⊤
main = putStrLn "Hello world!"

variable
 𝓤 𝓥 𝓦 𝓣 : Universe

{- | 𝟘
-}
data 𝟘 : 𝓤₀ ̇ where

𝟘-induction : (A : 𝟘 → 𝓤 ̇) → (x : 𝟘) → A x
𝟘-induction A ()

is-empty : 𝓤 ̇ → 𝓤 ̇
is-empty X = X → 𝟘

{- | 𝟙
-}
data 𝟙 : 𝓤₀ ̇ where
 ⋆ : 𝟙

𝟙-induction : (A : 𝟙 → 𝓤 ̇) → A ⋆ → (x : 𝟙) → A x
𝟙-induction A a ⋆ = a

𝟙-recursion : (A : 𝓤 ̇) → A → 𝟙 → A
𝟙-recursion A = 𝟙-induction (λ _ → A)

{- | _+_
-}
data _+_ {𝓤 𝓥} (X : 𝓤 ̇) (Y : 𝓥 ̇) : 𝓤 ⊔ 𝓥 ̇ where
 inl : X → X + Y
 inr : Y → X + Y

+-induction
 : {X : 𝓤 ̇} {Y : 𝓥 ̇} (A : X + Y → 𝓦 ̇)
 → ((x : X) → A (inl x))
 → ((y : Y) → A (inr y))
 → (z : X + Y)
 → A z
+-induction A f g (inl l) = f l
+-induction A f g (inr r) = g r

+-recursion
 : {X : 𝓤 ̇} {Y : 𝓥 ̇} {A : 𝓦 ̇}
 → (X → A)
 → (Y → A)
 → X + Y
 → A
+-recursion {𝓤} {𝓥} {𝓦} {X} {Y} {A} = +-induction (λ _ → A)

{- | 𝟚
-}
𝟚 : 𝓤₀ ̇
𝟚 = 𝟙 + 𝟙

𝟚-induction
 : (A : 𝟚 → 𝓤 ̇)
 → A (inl ⋆)
 → A (inr ⋆)
 → (n : 𝟚)
 → A n
𝟚-induction A a₀ a₁ = +-induction A
 (𝟙-induction (λ (x : 𝟙) → A (inl x)) a₀)
 (𝟙-induction (λ (y : 𝟙) → A (inr y)) a₁)

{- | Σ
-}
record Σ {𝓤 𝓥} {X : 𝓤 ̇} (Y : X → 𝓥 ̇) : 𝓤 ⊔ 𝓥 ̇ where
  constructor
   _,_
  field
   x : X
   y : Y x

-Σ : {𝓤 𝓥 : Universe} (X : 𝓤 ̇) (Y : X → 𝓥 ̇) → 𝓤 ⊔ 𝓥 ̇
-Σ X Y = Σ Y

syntax -Σ X (λ x → y) = Σ x ꞉ X , y

Σ-induction
 : {X : 𝓤 ̇} {Y : X → 𝓥 ̇} {A : Σ Y → 𝓦 ̇}
 → ((x : X) → (y : Y x) → A (x , y))
 → ((x , y) : Σ Y)
 → A (x , y)
Σ-induction g (x , y) = g x y

uncurry = Σ-induction

curry
 : {X : 𝓤 ̇} {Y : X → 𝓥 ̇} {A : Σ Y → 𝓦 ̇}
 → (((x , y) : Σ Y) → A (x , y))
 → ((x : X) (y : Y x) → A (x , y))
curry f x y = f (x , y)

Σ-recursion : 𝓤 ̇ → 𝓥 ̇ → 𝓤 ⊔ 𝓥 ̇
Σ-recursion X Y = Σ x ꞉ X , Y

_×_ = Σ-recursion

{- | Π
-}
Π : {X : 𝓤 ̇} (A : X → 𝓥 ̇) → 𝓤 ⊔ 𝓥 ̇
Π {𝓤} {𝓥} {X} A = (x : X) → A x

-Π : {𝓤 𝓥 : Universe} (X : 𝓤 ̇) (Y : X → 𝓥 ̇) → 𝓤 ⊔ 𝓥 ̇
-Π X Y = Π Y

syntax -Π A (λ x → b) = Π x ꞉ A , b

{- | Functions
-}
_∘_
 : {X : 𝓤 ̇} {Y : 𝓥 ̇} {Z : Y → 𝓦 ̇}
 → ((y : Y) → Z y)
 → (f : X → Y)
 → ((x : X) → Z (f x))
g ∘ f = λ x → g (f x)

domain
 : {X : 𝓤 ̇} {Y : 𝓥 ̇}
 → (X → Y)
 → 𝓤 ̇
domain {𝓤} {𝓥} {X} {Y} f = X

codomain
 : {X : 𝓤 ̇} {Y : 𝓥 ̇}
 → (X → Y)
 → 𝓥 ̇
codomain {𝓤} {𝓥} {X} {Y} f = Y

type-of : {X : 𝓤 ̇} → X → 𝓤 ̇
type-of {𝓤} {X} x = X

id : (X : 𝓤 ̇) → X → X
id X x = x

{- | Id
-}
data Id {𝓤} (X : 𝓤 ̇) : X → X → 𝓤 ̇ where
 refl : (x : X) → Id X x x

_==_ : {X : 𝓤 ̇} → X → X → 𝓤 ̇
x == y = Id _ x y

==-induction
 : (X : 𝓤 ̇) (A : (x y : X) → Id X x y → 𝓥 ̇)
 → ((x : X) → A x x (refl x))
 → (x y : X) (p : x == y)
 → A x y p
==-induction X A f x x (refl x) = f x

𝕁 = ==-induction

id-transitivity
 : {X : 𝓤 ̇} {x y z : X}
 → Id X x y
 → Id X y z
 → Id X x z
id-transitivity (refl xy) (refl yz) = refl xz
 where
  xz = xy

reverse-id : {X : 𝓤 ̇} {x y : X} → Id X x y → Id X y x
reverse-id (refl xy) = refl yx
 where yx = xy

{- | List
-}
data List (A : 𝓤 ̇) : 𝓤 ̇ where
 [] : List A
 _∷_ : A → List A → List A

infix 50 _==_
infixr 50 _,_
infixr 30 _×_
infixr 20 _+_
infixl 70 _∘_
infix   0 Id

-- not exactly correct
-- pullback :
-- (A B C : 𝓤 ̇) →
-- (f : A → C) → (g : B → C) → (a : A) → (b : B) → (f(a) == g(b))
-- pullback A B C f g a b = refl (f (a))

-- infix   0 _∼_
-- infix   0 _≡_
-- infix  10 _⇔_
-- infixl 30 _∙_
-- infixr  0 _≡⟨_⟩_
-- infix   1 _∎
-- infix  40 _⁻¹
-- infix  10 _◁_
-- infixr  0 _◁⟨_⟩_
-- infix   1 _◀
-- infix  10 _≃_
-- infixl 30 _●_
-- infixr  0 _≃⟨_⟩_
-- infix   1 _■
-- infix  40 _∈_
-- infix  30 _[_,_]
-- infixr -1 -Σ
-- infixr -1 -Π
-- infixr -1 -∃!
