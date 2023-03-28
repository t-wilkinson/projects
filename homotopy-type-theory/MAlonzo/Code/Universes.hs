{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module MAlonzo.Code.Universes where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Primitive

-- Universes.Type
d_Type_2 :: MAlonzo.Code.Agda.Primitive.T_Level_14 -> ()
d_Type_2 = erased
-- Universes._̇
d__'775'_8 :: MAlonzo.Code.Agda.Primitive.T_Level_14 -> ()
d__'775'_8 = erased
-- Universes.𝓤₁
d_𝓤'8321'_12 :: MAlonzo.Code.Agda.Primitive.T_Level_14
d_𝓤'8321'_12
  = coe
      MAlonzo.Code.Agda.Primitive.d_lsuc_20
      MAlonzo.Code.Agda.Primitive.d_lzero_16
-- Universes.𝓤₂
d_𝓤'8322'_14 :: MAlonzo.Code.Agda.Primitive.T_Level_14
d_𝓤'8322'_14
  = coe MAlonzo.Code.Agda.Primitive.d_lsuc_20 d_𝓤'8321'_12
-- Universes.𝓤₃
d_𝓤'8323'_16 :: MAlonzo.Code.Agda.Primitive.T_Level_14
d_𝓤'8323'_16
  = coe MAlonzo.Code.Agda.Primitive.d_lsuc_20 d_𝓤'8322'_14
-- Universes._⁺⁺
d__'8314''8314'_18 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14
d__'8314''8314'_18 v0
  = coe
      MAlonzo.Code.Agda.Primitive.d_lsuc_20
      (coe MAlonzo.Code.Agda.Primitive.d_lsuc_20 v0)
-- Universes.universe-of
d_universe'45'of_26 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Agda.Primitive.T_Level_14
d_universe'45'of_26 v0 ~v1 = du_universe'45'of_26 v0
du_universe'45'of_26 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14
du_universe'45'of_26 v0 = coe v0
