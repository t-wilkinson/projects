module Booth.Helpers where

import Quaalude
import Booth.Db
import Core.Config (Config, Environment(..), ServerKey
                   , booth, adminEmail, env
                   , stripeTestHook, stripeLiveHook, stripeTestKey, stripeLiveKey
                   )

isAdmin ∷ (MonadReader Config m) ⇒ User → m Bool
isAdmin user = do
    adminEmail' ← asks (^.booth.adminEmail)
    pure $ adminEmail' == userEmail user

{- Stripe Keys -}
getStripeHook, getStripeKey ∷ (MonadReader Config m) ⇒ m ServerKey
getStripeHook = do
    testHook ← asks (^.booth.stripeTestHook)
    liveHook ← asks (^.booth.stripeLiveHook)
    env' ← asks (^.env)
    pure case env' of
              Development → testHook
              Remote → testHook
              Production → liveHook

getStripeKey = do
    testKey ← asks (^.booth.stripeTestKey)
    liveKey ← asks (^.booth.stripeLiveKey)
    env' ← asks (^.env)
    pure case env' of
              Development → testKey
              Remote → testKey
              Production → liveKey
