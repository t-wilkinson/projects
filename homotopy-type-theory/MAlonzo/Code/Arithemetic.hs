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

module MAlonzo.Code.Arithemetic where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Primitive

-- Arithemetic.ℕ
d_ℕ_10 = ()
data T_ℕ_10 = C_zero_12 | C_succ_14 T_ℕ_10
-- Arithemetic.ℕ-induction
d_ℕ'45'induction_22 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (T_ℕ_10 -> ()) ->
  AgdaAny -> (T_ℕ_10 -> AgdaAny -> AgdaAny) -> T_ℕ_10 -> AgdaAny
d_ℕ'45'induction_22 ~v0 ~v1 v2 v3 v4
  = du_ℕ'45'induction_22 v2 v3 v4
du_ℕ'45'induction_22 ::
  AgdaAny -> (T_ℕ_10 -> AgdaAny -> AgdaAny) -> T_ℕ_10 -> AgdaAny
du_ℕ'45'induction_22 v0 v1 v2
  = case coe v2 of
      C_zero_12 -> coe v0
      C_succ_14 v3
        -> coe v1 v3 (coe du_ℕ'45'induction_22 (coe v0) (coe v1) (coe v3))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Arithemetic.ℕ-recursion
d_ℕ'45'recursion_36 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  AgdaAny -> (T_ℕ_10 -> AgdaAny -> AgdaAny) -> T_ℕ_10 -> AgdaAny
d_ℕ'45'recursion_36 ~v0 ~v1 = du_ℕ'45'recursion_36
du_ℕ'45'recursion_36 ::
  AgdaAny -> (T_ℕ_10 -> AgdaAny -> AgdaAny) -> T_ℕ_10 -> AgdaAny
du_ℕ'45'recursion_36 = coe du_ℕ'45'induction_22
-- Arithemetic.ℕ-iteration
d_ℕ'45'iteration_40 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> AgdaAny -> (AgdaAny -> AgdaAny) -> T_ℕ_10 -> AgdaAny
d_ℕ'45'iteration_40 ~v0 ~v1 v2 v3 = du_ℕ'45'iteration_40 v2 v3
du_ℕ'45'iteration_40 ::
  AgdaAny -> (AgdaAny -> AgdaAny) -> T_ℕ_10 -> AgdaAny
du_ℕ'45'iteration_40 v0 v1
  = coe du_ℕ'45'recursion_36 v0 (\ v2 -> v1)
-- Arithemetic.ℕ-induction'
d_ℕ'45'induction''_54 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (T_ℕ_10 -> ()) ->
  AgdaAny -> (T_ℕ_10 -> AgdaAny -> AgdaAny) -> T_ℕ_10 -> AgdaAny
d_ℕ'45'induction''_54 ~v0 ~v1 v2 v3 = du_ℕ'45'induction''_54 v2 v3
du_ℕ'45'induction''_54 ::
  AgdaAny -> (T_ℕ_10 -> AgdaAny -> AgdaAny) -> T_ℕ_10 -> AgdaAny
du_ℕ'45'induction''_54 v0 v1 = coe du_h_68 (coe v0) (coe v1)
-- Arithemetic._.h
d_h_68 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (T_ℕ_10 -> ()) ->
  AgdaAny -> (T_ℕ_10 -> AgdaAny -> AgdaAny) -> T_ℕ_10 -> AgdaAny
d_h_68 ~v0 ~v1 v2 v3 v4 = du_h_68 v2 v3 v4
du_h_68 ::
  AgdaAny -> (T_ℕ_10 -> AgdaAny -> AgdaAny) -> T_ℕ_10 -> AgdaAny
du_h_68 v0 v1 v2
  = case coe v2 of
      C_zero_12 -> coe v0
      C_succ_14 v3 -> coe v1 v3 (coe du_h_68 (coe v0) (coe v1) (coe v3))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Arithemetic.ℕ-recursion'
d_ℕ'45'recursion''_74 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  AgdaAny -> (T_ℕ_10 -> AgdaAny -> AgdaAny) -> T_ℕ_10 -> AgdaAny
d_ℕ'45'recursion''_74 ~v0 ~v1 = du_ℕ'45'recursion''_74
du_ℕ'45'recursion''_74 ::
  AgdaAny -> (T_ℕ_10 -> AgdaAny -> AgdaAny) -> T_ℕ_10 -> AgdaAny
du_ℕ'45'recursion''_74 = coe du_ℕ'45'induction''_54
-- Arithemetic.ℕ-iteration'
d_ℕ'45'iteration''_82 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> AgdaAny -> (AgdaAny -> AgdaAny) -> T_ℕ_10 -> AgdaAny
d_ℕ'45'iteration''_82 ~v0 ~v1 v2 v3 = du_ℕ'45'iteration''_82 v2 v3
du_ℕ'45'iteration''_82 ::
  AgdaAny -> (AgdaAny -> AgdaAny) -> T_ℕ_10 -> AgdaAny
du_ℕ'45'iteration''_82 v0 v1
  = coe du_ℕ'45'recursion''_74 v0 (\ v2 -> v1)
-- Arithemetic.Arithmetic._+_
d__'43'__96 :: T_ℕ_10 -> T_ℕ_10 -> T_ℕ_10
d__'43'__96 v0 v1 = coe du_ℕ'45'iteration_40 v0 (coe C_succ_14) v1
-- Arithemetic.Arithmetic._×_
d__'215'__98 :: T_ℕ_10 -> T_ℕ_10 -> T_ℕ_10
d__'215'__98 v0 v1
  = coe du_ℕ'45'iteration_40 v0 (d__'43'__96 (coe v0)) v1
