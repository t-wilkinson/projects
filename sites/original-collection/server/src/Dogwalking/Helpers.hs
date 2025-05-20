module Dogwalking.Helpers where

import Quaalude
import Dogwalking.Db
import Core.Config (Config, Environment(..), ServerKey
                   , dogwalking, adminEmail , env
                   , stripeTestHook, stripeLiveHook, stripeTestKey, stripeLiveKey
                   )

isAdmin ∷ (MonadReader Config m) ⇒ User → m Bool
isAdmin user = do
    adminEmail' ← asks (^.dogwalking.adminEmail)
    pure $ adminEmail' == userEmail user

{- Stripe Keys -}
getStripeHook, getStripeKey ∷ (MonadReader Config m) ⇒ m ServerKey
getStripeHook = do
    testHook ← asks (^.dogwalking.stripeTestHook)
    liveHook ← asks (^.dogwalking.stripeLiveHook)
    env' ← asks (^.env)
    pure case env' of
              Development → testHook
              Remote → testHook
              Production → liveHook

getStripeKey = do
    testKey ← asks (^.dogwalking.stripeTestKey)
    liveKey ← asks (^.dogwalking.stripeLiveKey)
    env' ← asks (^.env)
    pure case env' of
              Development → testKey
              Remote → testKey
              Production → liveKey
