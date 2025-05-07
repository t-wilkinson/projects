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

module MAlonzo.Code.NaturalNumbers where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Primitive

-- NaturalNumbers.𝟙
d_𝟙_2 = ()
data T_𝟙_2 = C_'8902'_4
-- NaturalNumbers.𝟘
d_𝟘_6 = ()
data T_𝟘_6
-- NaturalNumbers.ℕ
d_ℕ_16 = ()
data T_ℕ_16 = C_zero_18 | C_succ_20 T_ℕ_16
-- NaturalNumbers.ℕ-induction
d_ℕ'45'induction_28 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (T_ℕ_16 -> ()) ->
  AgdaAny -> (T_ℕ_16 -> AgdaAny -> AgdaAny) -> T_ℕ_16 -> AgdaAny
d_ℕ'45'induction_28 ~v0 ~v1 v2 v3 v4
  = du_ℕ'45'induction_28 v2 v3 v4
du_ℕ'45'induction_28 ::
  AgdaAny -> (T_ℕ_16 -> AgdaAny -> AgdaAny) -> T_ℕ_16 -> AgdaAny
du_ℕ'45'induction_28 v0 v1 v2
  = case coe v2 of
      C_zero_18 -> coe v0
      C_succ_20 v3
        -> coe v1 v3 (coe du_ℕ'45'induction_28 (coe v0) (coe v1) (coe v3))
      _ -> MAlonzo.RTE.mazUnreachableError
-- NaturalNumbers.ℕ-recursion
d_ℕ'45'recursion_42 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  AgdaAny -> (T_ℕ_16 -> AgdaAny -> AgdaAny) -> T_ℕ_16 -> AgdaAny
d_ℕ'45'recursion_42 ~v0 ~v1 = du_ℕ'45'recursion_42
du_ℕ'45'recursion_42 ::
  AgdaAny -> (T_ℕ_16 -> AgdaAny -> AgdaAny) -> T_ℕ_16 -> AgdaAny
du_ℕ'45'recursion_42 = coe du_ℕ'45'induction_28
-- NaturalNumbers.ℕ-iteration
d_ℕ'45'iteration_46 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> AgdaAny -> (AgdaAny -> AgdaAny) -> T_ℕ_16 -> AgdaAny
d_ℕ'45'iteration_46 ~v0 ~v1 v2 v3 = du_ℕ'45'iteration_46 v2 v3
du_ℕ'45'iteration_46 ::
  AgdaAny -> (AgdaAny -> AgdaAny) -> T_ℕ_16 -> AgdaAny
du_ℕ'45'iteration_46 v0 v1
  = coe du_ℕ'45'recursion_42 v0 (\ v2 -> v1)
-- NaturalNumbers.ℕ-induction'
d_ℕ'45'induction''_60 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (T_ℕ_16 -> ()) ->
  AgdaAny -> (T_ℕ_16 -> AgdaAny -> AgdaAny) -> T_ℕ_16 -> AgdaAny
d_ℕ'45'induction''_60 ~v0 ~v1 v2 v3 = du_ℕ'45'induction''_60 v2 v3
du_ℕ'45'induction''_60 ::
  AgdaAny -> (T_ℕ_16 -> AgdaAny -> AgdaAny) -> T_ℕ_16 -> AgdaAny
du_ℕ'45'induction''_60 v0 v1 = coe du_h_74 (coe v0) (coe v1)
-- NaturalNumbers._.h
d_h_74 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (T_ℕ_16 -> ()) ->
  AgdaAny -> (T_ℕ_16 -> AgdaAny -> AgdaAny) -> T_ℕ_16 -> AgdaAny
d_h_74 ~v0 ~v1 v2 v3 v4 = du_h_74 v2 v3 v4
du_h_74 ::
  AgdaAny -> (T_ℕ_16 -> AgdaAny -> AgdaAny) -> T_ℕ_16 -> AgdaAny
du_h_74 v0 v1 v2
  = case coe v2 of
      C_zero_18 -> coe v0
      C_succ_20 v3 -> coe v1 v3 (coe du_h_74 (coe v0) (coe v1) (coe v3))
      _ -> MAlonzo.RTE.mazUnreachableError
-- NaturalNumbers.ℕ-recursion'
d_ℕ'45'recursion''_80 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  AgdaAny -> (T_ℕ_16 -> AgdaAny -> AgdaAny) -> T_ℕ_16 -> AgdaAny
d_ℕ'45'recursion''_80 ~v0 ~v1 = du_ℕ'45'recursion''_80
du_ℕ'45'recursion''_80 ::
  AgdaAny -> (T_ℕ_16 -> AgdaAny -> AgdaAny) -> T_ℕ_16 -> AgdaAny
du_ℕ'45'recursion''_80 = coe du_ℕ'45'induction''_60
-- NaturalNumbers.ℕ-iteration'
d_ℕ'45'iteration''_88 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> AgdaAny -> (AgdaAny -> AgdaAny) -> T_ℕ_16 -> AgdaAny
d_ℕ'45'iteration''_88 ~v0 ~v1 v2 v3 = du_ℕ'45'iteration''_88 v2 v3
du_ℕ'45'iteration''_88 ::
  AgdaAny -> (AgdaAny -> AgdaAny) -> T_ℕ_16 -> AgdaAny
du_ℕ'45'iteration''_88 v0 v1
  = coe du_ℕ'45'recursion''_80 v0 (\ v2 -> v1)
-- NaturalNumbers.Arithmetic._+_
d__'43'__102 :: T_ℕ_16 -> T_ℕ_16 -> T_ℕ_16
d__'43'__102 v0 v1
  = case coe v1 of
      C_zero_18 -> coe v0
      C_succ_20 v2 -> coe C_succ_20 (coe d__'43'__102 (coe v0) (coe v2))
      _ -> MAlonzo.RTE.mazUnreachableError
-- NaturalNumbers.Arithmetic._×_
d__'215'__104 :: T_ℕ_16 -> T_ℕ_16 -> T_ℕ_16
d__'215'__104 v0 v1
  = case coe v1 of
      C_zero_18 -> coe v1
      C_succ_20 v2
        -> coe d__'43'__102 (coe v0) (coe d__'215'__104 (coe v0) (coe v2))
      _ -> MAlonzo.RTE.mazUnreachableError
-- NaturalNumbers.Arithmetic'._+_
d__'43'__120 :: T_ℕ_16 -> T_ℕ_16 -> T_ℕ_16
d__'43'__120 v0 v1 = coe du_ℕ'45'iteration_46 v0 (coe C_succ_20) v1
-- NaturalNumbers.Arithmetic'._×_
d__'215'__122 :: T_ℕ_16 -> T_ℕ_16 -> T_ℕ_16
d__'215'__122 v0 v1
  = coe
      du_ℕ'45'iteration_46 (coe C_zero_18) (d__'43'__120 (coe v0)) v1
-- NaturalNumbers.ℕ-order._≤_
d__'8804'__136 :: T_ℕ_16 -> T_ℕ_16 -> ()
d__'8804'__136 = erased
-- NaturalNumbers.ℕ-order._≥_
d__'8805'__138 :: T_ℕ_16 -> T_ℕ_16 -> ()
d__'8805'__138 = erased
