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

module MAlonzo.Code.HoTT where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.IO
import qualified MAlonzo.Code.Agda.Builtin.String
import qualified MAlonzo.Code.Agda.Builtin.Unit
import qualified MAlonzo.Code.Agda.Primitive

import qualified Data.Text as T
-- HoTT.putStrLn
d_putStrLn_2 ::
  MAlonzo.Code.Agda.Builtin.String.T_String_6 ->
  MAlonzo.Code.Agda.Builtin.IO.T_IO_8
    () MAlonzo.Code.Agda.Builtin.Unit.T_'8868'_6
d_putStrLn_2 = putStrLn . T.unpack
-- HoTT.main
d_main_4 ::
  MAlonzo.Code.Agda.Builtin.IO.T_IO_8
    AgdaAny MAlonzo.Code.Agda.Builtin.Unit.T_'8868'_6
d_main_4 = coe d_putStrLn_2 ("Hello world!" :: Data.Text.Text)
-- HoTT.𝟘
d_𝟘_14 = ()
data T_𝟘_14
-- HoTT.𝟘-induction
d_𝟘'45'induction_20 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (T_𝟘_14 -> ()) -> T_𝟘_14 -> AgdaAny
d_𝟘'45'induction_20 ~v0 ~v1 ~v2 = du_𝟘'45'induction_20
du_𝟘'45'induction_20 :: AgdaAny
du_𝟘'45'induction_20 = MAlonzo.RTE.mazUnreachableError
-- HoTT.is-empty
d_is'45'empty_24 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()
d_is'45'empty_24 = erased
-- HoTT.𝟙
d_𝟙_28 = ()
data T_𝟙_28 = C_'8902'_30
-- HoTT.𝟙-induction
d_𝟙'45'induction_36 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (T_𝟙_28 -> ()) -> AgdaAny -> T_𝟙_28 -> AgdaAny
d_𝟙'45'induction_36 ~v0 ~v1 v2 ~v3 = du_𝟙'45'induction_36 v2
du_𝟙'45'induction_36 :: AgdaAny -> AgdaAny
du_𝟙'45'induction_36 v0 = coe v0
-- HoTT.𝟙-recursion
d_𝟙'45'recursion_44 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> AgdaAny -> T_𝟙_28 -> AgdaAny
d_𝟙'45'recursion_44 ~v0 ~v1 v2 ~v3 = du_𝟙'45'recursion_44 v2
du_𝟙'45'recursion_44 :: AgdaAny -> AgdaAny
du_𝟙'45'recursion_44 v0 = coe v0
-- HoTT._+_
d__'43'__58 a0 a1 a2 a3 = ()
data T__'43'__58 = C_inl_68 AgdaAny | C_inr_70 AgdaAny
-- HoTT.+-induction
d_'43''45'induction_84 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  (T__'43'__58 -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> T__'43'__58 -> AgdaAny
d_'43''45'induction_84 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 v7 v8
  = du_'43''45'induction_84 v6 v7 v8
du_'43''45'induction_84 ::
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> T__'43'__58 -> AgdaAny
du_'43''45'induction_84 v0 v1 v2
  = case coe v2 of
      C_inl_68 v3 -> coe v0 v3
      C_inr_70 v3 -> coe v1 v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- HoTT.+-recursion
d_'43''45'recursion_108 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  () ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> T__'43'__58 -> AgdaAny
d_'43''45'recursion_108 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5
  = du_'43''45'recursion_108
du_'43''45'recursion_108 ::
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> T__'43'__58 -> AgdaAny
du_'43''45'recursion_108 = coe du_'43''45'induction_84
-- HoTT.𝟚
d_𝟚_124 :: ()
d_𝟚_124 = erased
-- HoTT.𝟚-induction
d_𝟚'45'induction_130 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (T__'43'__58 -> ()) -> AgdaAny -> AgdaAny -> T__'43'__58 -> AgdaAny
d_𝟚'45'induction_130 ~v0 ~v1 v2 v3 = du_𝟚'45'induction_130 v2 v3
du_𝟚'45'induction_130 ::
  AgdaAny -> AgdaAny -> T__'43'__58 -> AgdaAny
du_𝟚'45'induction_130 v0 v1
  = coe du_'43''45'induction_84 (coe (\ v2 -> v0)) (coe (\ v2 -> v1))
-- HoTT.Σ
d_Σ_150 a0 a1 a2 a3 = ()
data T_Σ_150 = C__'44'__168 AgdaAny AgdaAny
-- HoTT.Σ.x
d_x_164 :: T_Σ_150 -> AgdaAny
d_x_164 v0
  = case coe v0 of
      C__'44'__168 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- HoTT.Σ.y
d_y_166 :: T_Σ_150 -> AgdaAny
d_y_166 v0
  = case coe v0 of
      C__'44'__168 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- HoTT.-Σ
d_'45'Σ_178 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> (AgdaAny -> ()) -> ()
d_'45'Σ_178 = erased
-- HoTT.Σ-induction
d_Σ'45'induction_200 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> ()) ->
  (T_Σ_150 -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> T_Σ_150 -> AgdaAny
d_Σ'45'induction_200 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 v7
  = du_Σ'45'induction_200 v6 v7
du_Σ'45'induction_200 ::
  (AgdaAny -> AgdaAny -> AgdaAny) -> T_Σ_150 -> AgdaAny
du_Σ'45'induction_200 v0 v1
  = case coe v1 of
      C__'44'__168 v2 v3 -> coe v0 v2 v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- HoTT.uncurry
d_uncurry_208 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> ()) ->
  (T_Σ_150 -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> T_Σ_150 -> AgdaAny
d_uncurry_208 v0 v1 v2 v3 v4 v5 v6 v7
  = coe du_Σ'45'induction_200 v6 v7
-- HoTT.curry
d_curry_226 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> ()) ->
  (T_Σ_150 -> ()) ->
  (T_Σ_150 -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
d_curry_226 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 v7 v8
  = du_curry_226 v6 v7 v8
du_curry_226 ::
  (T_Σ_150 -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
du_curry_226 v0 v1 v2 = coe v0 (coe C__'44'__168 (coe v1) (coe v2))
-- HoTT.Σ-recursion
d_Σ'45'recursion_234 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> () -> ()
d_Σ'45'recursion_234 = erased
-- HoTT._×_
d__'215'__242 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> () -> ()
d__'215'__242 = erased
-- HoTT.Π
d_Π_248 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> (AgdaAny -> ()) -> ()
d_Π_248 = erased
-- HoTT.-Π
d_'45'Π_268 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> (AgdaAny -> ()) -> ()
d_'45'Π_268 = erased
-- HoTT._∘_
d__'8728'__286 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  (AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d__'8728'__286 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 v7 v8
  = du__'8728'__286 v6 v7 v8
du__'8728'__286 ::
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du__'8728'__286 v0 v1 v2 = coe v0 (coe v1 v2)
-- HoTT.domain
d_domain_298 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> () -> (AgdaAny -> AgdaAny) -> ()
d_domain_298 = erased
-- HoTT.codomain
d_codomain_314 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> () -> (AgdaAny -> AgdaAny) -> ()
d_codomain_314 = erased
-- HoTT.type-of
d_type'45'of_328 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> AgdaAny -> ()
d_type'45'of_328 = erased
-- HoTT.id
d_id_338 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> AgdaAny -> AgdaAny
d_id_338 ~v0 ~v1 v2 = du_id_338 v2
du_id_338 :: AgdaAny -> AgdaAny
du_id_338 v0 = coe v0
-- HoTT.Id
d_Id_348 a0 a1 a2 a3 = ()
data T_Id_348 = C_refl_356
-- HoTT._==_
d__'61''61'__360 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> AgdaAny -> AgdaAny -> ()
d__'61''61'__360 = erased
-- HoTT.==-induction
d_'61''61''45'induction_382 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> T_Id_348 -> ()) ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> T_Id_348 -> AgdaAny
d_'61''61''45'induction_382 ~v0 ~v1 ~v2 ~v3 v4 v5 ~v6 v7
  = du_'61''61''45'induction_382 v4 v5 v7
du_'61''61''45'induction_382 ::
  (AgdaAny -> AgdaAny) -> AgdaAny -> T_Id_348 -> AgdaAny
du_'61''61''45'induction_382 v0 v1 v2
  = coe seq (coe v2) (coe v0 v1)
-- HoTT.𝕁
d_𝕁_392 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> T_Id_348 -> ()) ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> T_Id_348 -> AgdaAny
d_𝕁_392 v0 v1 v2 v3 v4 v5 v6 v7
  = coe du_'61''61''45'induction_382 v4 v5 v7
-- HoTT.id-transitivity
d_id'45'transitivity_402 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  AgdaAny -> AgdaAny -> AgdaAny -> T_Id_348 -> T_Id_348 -> T_Id_348
d_id'45'transitivity_402 ~v0 ~v1 ~v2 ~v3 ~v4 v5 v6
  = du_id'45'transitivity_402 v5 v6
du_id'45'transitivity_402 :: T_Id_348 -> T_Id_348 -> T_Id_348
du_id'45'transitivity_402 v0 v1
  = coe seq (coe v0) (coe seq (coe v1) (coe C_refl_356))
-- HoTT._.xz
d_xz_412 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> AgdaAny -> AgdaAny
d_xz_412 ~v0 ~v1 v2 = du_xz_412 v2
du_xz_412 :: AgdaAny -> AgdaAny
du_xz_412 v0 = coe v0
-- HoTT.reverse-id
d_reverse'45'id_420 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> AgdaAny -> AgdaAny -> T_Id_348 -> T_Id_348
d_reverse'45'id_420 ~v0 ~v1 ~v2 ~v3 v4 = du_reverse'45'id_420 v4
du_reverse'45'id_420 :: T_Id_348 -> T_Id_348
du_reverse'45'id_420 v0 = coe seq (coe v0) (coe C_refl_356)
-- HoTT._.yx
d_yx_428 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> AgdaAny -> AgdaAny
d_yx_428 ~v0 ~v1 v2 = du_yx_428 v2
du_yx_428 :: AgdaAny -> AgdaAny
du_yx_428 v0 = coe v0
-- HoTT.List
d_List_434 a0 a1 = ()
data T_List_434
  = C_'91''93'_438 | C__'8759'__440 AgdaAny T_List_434
