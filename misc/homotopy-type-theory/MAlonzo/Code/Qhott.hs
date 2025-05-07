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

module MAlonzo.Code.Qhott where

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
-- hott.putStrLn
d_putStrLn_2 ::
  MAlonzo.Code.Agda.Builtin.String.T_String_6 ->
  MAlonzo.Code.Agda.Builtin.IO.T_IO_8
    () MAlonzo.Code.Agda.Builtin.Unit.T_'8868'_6
d_putStrLn_2 = putStrLn . T.unpack
main = coe d_main_4
-- hott.main
d_main_4 ::
  MAlonzo.Code.Agda.Builtin.IO.T_IO_8
    AgdaAny MAlonzo.Code.Agda.Builtin.Unit.T_'8868'_6
d_main_4 = coe d_putStrLn_2 ("Hello world!" :: Data.Text.Text)
-- hott.𝟘
d_𝟘_14 = ()
data T_𝟘_14
-- hott.𝟘-induction
d_𝟘'45'induction_20 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (T_𝟘_14 -> ()) -> T_𝟘_14 -> AgdaAny
d_𝟘'45'induction_20 ~v0 ~v1 ~v2 = du_𝟘'45'induction_20
du_𝟘'45'induction_20 :: AgdaAny
du_𝟘'45'induction_20 = MAlonzo.RTE.mazUnreachableError
-- hott.is-empty
d_is'45'empty_24 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()
d_is'45'empty_24 = erased
-- hott.𝟙
d_𝟙_28 = ()
data T_𝟙_28 = C_'8902'_30
-- hott.𝟙-induction
d_𝟙'45'induction_36 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (T_𝟙_28 -> ()) -> AgdaAny -> T_𝟙_28 -> AgdaAny
d_𝟙'45'induction_36 ~v0 ~v1 v2 ~v3 = du_𝟙'45'induction_36 v2
du_𝟙'45'induction_36 :: AgdaAny -> AgdaAny
du_𝟙'45'induction_36 v0 = coe v0
-- hott.𝟙-recursion
d_𝟙'45'recursion_44 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> AgdaAny -> T_𝟙_28 -> AgdaAny
d_𝟙'45'recursion_44 ~v0 ~v1 v2 ~v3 = du_𝟙'45'recursion_44 v2
du_𝟙'45'recursion_44 :: AgdaAny -> AgdaAny
du_𝟙'45'recursion_44 v0 = coe v0
-- hott._+_
d__'43'__58 a0 a1 a2 a3 = ()
data T__'43'__58 = C_inl_68 AgdaAny | C_inr_70 AgdaAny
-- hott.+-induction
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
-- hott.+-recursion
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
-- hott.𝟚
d_𝟚_124 :: ()
d_𝟚_124 = erased
-- hott.𝟚-induction
d_𝟚'45'induction_130 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (T__'43'__58 -> ()) -> AgdaAny -> AgdaAny -> T__'43'__58 -> AgdaAny
d_𝟚'45'induction_130 ~v0 ~v1 v2 v3 = du_𝟚'45'induction_130 v2 v3
du_𝟚'45'induction_130 ::
  AgdaAny -> AgdaAny -> T__'43'__58 -> AgdaAny
du_𝟚'45'induction_130 v0 v1
  = coe du_'43''45'induction_84 (coe (\ v2 -> v0)) (coe (\ v2 -> v1))
-- hott.Σ
d_Σ_150 a0 a1 a2 a3 = ()
data T_Σ_150 = C__'44'__168 AgdaAny AgdaAny
-- hott.Σ.x
d_x_164 :: T_Σ_150 -> AgdaAny
d_x_164 v0
  = case coe v0 of
      C__'44'__168 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- hott.Σ.y
d_y_166 :: T_Σ_150 -> AgdaAny
d_y_166 v0
  = case coe v0 of
      C__'44'__168 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- hott.-Σ
d_'45'Σ_178 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> (AgdaAny -> ()) -> ()
d_'45'Σ_178 = erased
-- hott.Σ-induction
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
-- hott.uncurry
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
-- hott.curry
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
-- hott.Σ-recursion
d_Σ'45'recursion_234 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> () -> ()
d_Σ'45'recursion_234 = erased
-- hott._×_
d__'215'__242 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> () -> ()
d__'215'__242 = erased
-- hott.Π
d_Π_248 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> (AgdaAny -> ()) -> ()
d_Π_248 = erased
-- hott.-Π
d_'45'Π_268 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> (AgdaAny -> ()) -> ()
d_'45'Π_268 = erased
-- hott._∘_
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
-- hott.domain
d_domain_298 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> () -> (AgdaAny -> AgdaAny) -> ()
d_domain_298 = erased
-- hott.codomain
d_codomain_314 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> () -> (AgdaAny -> AgdaAny) -> ()
d_codomain_314 = erased
-- hott.type-of
d_type'45'of_328 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> AgdaAny -> ()
d_type'45'of_328 = erased
-- hott.id
d_id_338 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> AgdaAny -> AgdaAny
d_id_338 ~v0 ~v1 v2 = du_id_338 v2
du_id_338 :: AgdaAny -> AgdaAny
du_id_338 v0 = coe v0
-- hott.Id
d_Id_348 a0 a1 a2 a3 = ()
data T_Id_348 = C_refl_356
-- hott._==_
d__'61''61'__360 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> AgdaAny -> AgdaAny -> ()
d__'61''61'__360 = erased
-- hott.List
d_List_370 a0 a1 = ()
data T_List_370
  = C_'91''93'_374 | C__'8759'__376 AgdaAny T_List_370
