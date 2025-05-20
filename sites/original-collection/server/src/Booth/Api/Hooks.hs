module Booth.Api.Hooks where

import Quaalude
import Core.App (AppS)
import Core.Config (Config)
import Core.Helpers (mayFromJSON)
import Booth.Db
import Booth.Db.Types
import Booth.Service (getOverlappingServices)
import qualified Core.Errors as CE

import Network.Wai (Request(..))
import qualified Data.Aeson.Lens as JSON
import qualified Database.Esqueleto as E
import qualified Database.Persist as P
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.Text as Text

data Route r = Route
    { _paymentIntent ∷ r
        :- "payment_intent"
        :> Post '[JSON] ()
    } deriving (Generic)

route ∷ (Request, BS8.ByteString) → Route AppS
route req = Route @AppS
    (paymentIntentSucceeded req)

paymentIntentSucceeded
    ∷ ∀ m.
        ( MonadIO m
        , MonadError ServerError m
        , MonadReader Config m
        )
    ⇒ (Request, BS8.ByteString)
    → m ()
paymentIntentSucceeded (req, body) = do
    -- Gather 'customerId' and 'amount'
    liftIO $ putStrLn "paymentIntentSucceeded - start"
    let composeKeys pis = foldr (\x acc → acc . JSON.key x) (JSON.key "data" . JSON.key "object") $ reverse pis
        project ∷ (FromJSON a) ⇒ [Text.Text] → m a
        project pis = maybe (throwError CE.parseRequest) pure $ mayFromJSON $ body ^? composeKeys pis
        projectString ∷ (FromJSON a) ⇒ [Text.Text] → m a
        projectString pis = maybe (throwError CE.parseRequest) pure $ do
                v ∷ String ← mayFromJSON $ body ^? composeKeys pis
                decode $ BSL8.pack v

    liftIO $ putStrLn "paymentIntentSucceeded - after let"

    eventType ← maybe (throwError CE.parseRequest) pure $ mayFromJSON $ body ^? JSON.key "type"
    liftIO $ print eventType

    case eventType ∷ String of
        "payment_intent.succeeded" → do
            amount ← project ["amount"]
            customerId ← project ["customer"]
            serviceId ← projectString ["metadata","serviceId"]
            paymentType ← projectString ["metadata","paymentType"]

            case paymentType of
                ServiceFee → (runDb $ P.get serviceId) >>= \case
                    -- Only add service when does not overlap with existing services
                    Just Service{..} → do
                        let Date{..} = serviceDate
                        (getOverlappingServices (Just serviceId) dateYear dateMonth dateDay serviceStart serviceEnd) >>= \case
                            [] → runDb $ P.update serviceId [ServiceStatus P.=. Created]
                            _ → throwError CE.stripeHookService
                    Nothing → throwError CE.stripeHookService

                AccountCharge → do
                    -- Update 'UserTotalPayed'
                    users ← runDb $ E.select $ E.from \u → do
                        E.where_ $ u E.^. UserCustomerId E.==. E.val customerId
                        pure u
                    case users of
                        (u:[]) → runDb $ P.update (P.entityKey u) [UserTotalPayed P.+=. amount]
                        [] → throwError CE.userDNE
                        _ → throwError CE.tooManyUsers

        "payment_intent.processing" → do
            serviceId ← project ["metadata","serviceId"]
            runDb $ P.update serviceId [ServiceStatus P.=. PaymentProcessing]

        "payment_intent.payment_failed" → do
            serviceId ← project ["metadata","serviceId"]
            runDb $ P.update serviceId [ServiceStatus P.=. Failed]

        _ → pure ()

    lifIO $ putStrLn "paymentIntentSucceeded - end"
    pure ()
