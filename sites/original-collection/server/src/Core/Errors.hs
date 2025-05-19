{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Core.Errors where

import Quaalude
import qualified Data.ByteString.Lazy.Char8 as BSL8

-- sort /err/
-- sort d

headers            = [("Content-Type", "application/json")]
-- 'show' body to surround string with quotes so it parses as json
err_ e body = e { errHeaders = headers, errBody = BSL8.pack $ show (body ∷ String) }

accountExists       = err_ err409 "Account already exists."
addressExists       = err_ err409 "Address already exists."
badTime             = err_ err400 "Invalid time. Service must occur in the future."
invalidCredentials  = err_ err401 "Invalid credentials."
invalidService      = err_ err409 "Invalid service."
invalidSession      = err_ err401 "Unable to validate session."
noRates             = err_ err400 "Could not find rates for service."
notAdmin            = err_ err401 ""
parseRequest        = err_ err400 "Could not parse request."
parsingCredentials  = err_ err400 "Failure parsing email/password."
paymentMethod       = err_ err404 "Payment method not found."
serverKeyMatch      = err_ err400 "Server Keys do not match up."
serviceDNE          = err_ err404 "Could not find service in servers."
stripeHookService   = err_ err404 "Could not find service."
stripeConnection    = err_ err500 "Trouble connecting with Stripe servers."
stripeData          = err_ err400 "Stripe could not parse request."
stripeSignature     = err_ err400 "Could not validate signature."
serviceInPast       = err_ err400 "Service must be added in the future."
serviceOverlapping  = err_ err400 "Service is overlapping with existing ones."
stripeUser          = err_ err400 "Stripe could not handle your data."
token               = err_ err400 "Could not validate token."
tooManyUsers        = err_ err400 "Too many users found in server."
userDNE             = err_ err404 "Could not find user in servers."
validatingCookie    = err_ err401 "Could not validate session cookie."
backwardsTimes      = err_ err409 "Start time must precede end time."
