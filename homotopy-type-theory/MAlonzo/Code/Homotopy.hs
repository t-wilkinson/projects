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

module MAlonzo.Code.Homotopy where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.HoTT

-- Homotopy.S₁
d_S'8321'_2 = ()
data T_S'8321'_2 = C_base_4 | C_loop_6 MAlonzo.Code.HoTT.T_Id_348
-- Homotopy.double-loop
d_double'45'loop_8 :: T_S'8321'_2
d_double'45'loop_8
  = coe
      C_loop_6
      (coe
         MAlonzo.Code.HoTT.du_id'45'transitivity_402
         (coe MAlonzo.Code.HoTT.C_refl_356)
         (coe MAlonzo.Code.HoTT.C_refl_356))
-- Homotopy.reverse-S₁
d_reverse'45'S'8321'_10 :: T_S'8321'_2 -> T_S'8321'_2
d_reverse'45'S'8321'_10 v0
  = case coe v0 of
      C_base_4 -> coe v0
      C_loop_6 v1
        -> coe
             seq (coe v1)
             (coe
                C_loop_6
                (coe
                   MAlonzo.Code.HoTT.du_reverse'45'id_420
                   (coe MAlonzo.Code.HoTT.C_refl_356)))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Homotopy.homotopy
d_homotopy_14 :: MAlonzo.Code.HoTT.T_Id_348
d_homotopy_14 = coe MAlonzo.Code.HoTT.C_refl_356
