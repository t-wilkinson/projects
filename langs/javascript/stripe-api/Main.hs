data AddPaymentMethod = AddPaymentMethod
    { paymentIntentId ∷ String
    , cardNumber ∷ Word
    , cardCvc ∷ Word
    , cardMonth ∷ Word
    , cardYear ∷ Word
    , postalCode ∷ Word
    } deriving (Generic)
      deriving anyclass (ToJSON, FromJSON)

data ConfirmPaymentMethod = ConfirmPaymentMethod
    { paymentIntentId ∷ String
    } deriving (Generic)
      deriving anyclass (ToJSON, FromJSON)

data UsePaymentMethod = UsePaymentMethod
    { paymentMethodKey ∷ E.Key PaymentMethod
    } deriving (Generic)
      deriving anyclass (ToJSON, FromJSON)


data Route = Route
    { _makeCharge ∷ r
        :- "charge"
        :> Description "Charge users using payment method the amount they owe."
        :> Capture "id" Token
        :> ReqBody '[JSON] UsePaymentMethod
        :> Post '[JSON] ()

    , _confirmPaymentIntent ∷ r
        :- "intent"
        :> Description "Confirm that the payment intent is valid."
        :> Capture "id" Token
        :> ReqBody '[JSON] ConfirmPaymentMethod
        :> Put '[JSON] ()

    , _addPaymentMethod ∷ r
        :- "method"
        :> Description "Add a payment method to the user."
        :> ReqBody '[JSON] AddPaymentMethod
        :> Post '[JSON] String
    , _getPaymentMethods ∷ r
        :- "method"
        :> Description "Get all of the users payment methods so they can select one."
        :> Get '[JSON] [E.Key PaymentMethod]
    } deriving (Generic)


makeCharge
    ∷   ( MonadIO m
        , MonadError ServerError m
        , MonadReader Config m
        )
    ⇒ E.Entity User
    → Token
    → UsePaymentMethod
    → m ()
makeCharge entityUser token UsePaymentMethod{paymentMethodKey} = do
    -- Validate token
    sk ← asks (^.sk)
    key ← asks (^.booth.stripeKey)
    unless (validateToken sk token paymentMethodKey) $ throwError CE.serverKeyMatch

    -- Generate request body
    let customerId = userCustomerId $ E.entityVal entityUser
    charge ← getUserCharge entityUser
    methodId ← (runDb $ E.get paymentMethodKey) >>= \case
        Nothing → throwError CE.paymentMethod
        Just x → pure $ paymentMethodPaymentId x

    let req =
            [ ("payment_method", methodId)
            , ("customer", customerId)
            , ("amount", show charge)
            , ("currency", "usd")
            , ("confirm", "true")
            ]
    mayBody ← stripeReq key req POST ["v1","payment_intents"]
    when (isNothing mayBody) $ throwError CE.stripeConnection
    pure ()

getPaymentMethods
    ∷   ( MonadIO m
        , MonadReader Config m
        )
   ⇒ E.Entity User
   → m [E.Key PaymentMethod]
getPaymentMethods entityUser = do
    userPaymentMethods ← runDb $ E.select $ E.from \user → do
        E.where_ $ user E.^. PaymentMethodUId E.==. E.val (E.entityKey entityUser)
        pure user
    pure $ fmap E.entityKey userPaymentMethods


addPaymentMethod
    ∷   ( MonadIO m
        , MonadReader Config m
        , MonadError ServerError m
        )
    ⇒ E.Entity User
    → AddPaymentMethod
    → m String
addPaymentMethod E.Entity{..} AddPaymentMethod{..} = do
    -- attach payment method to customer in api.stripe and DB
    let customerId = userCustomerId entityVal
    key ← asks (^.booth.stripeKey)
    sk ← asks (^.sk)

    let req =
            [ ("type", "card")
            , ("card[number]", show cardNumber)
            , ("card[exp_month]", show cardMonth)
            , ("card[exp_year]", show cardYear)
            , ("card[cvc]", show cardCvc)
            , ("billing_details[address][postal_code]", show postalCode)
            , ("billing_details[email]", userEmail entityVal)
            ]
    res ← stripeReqBS key req POST ["v1","payment_methods"]
    paymentMethodId ← parseStripeRes $ res ^? JSON.key "id"

    let req =
            [ ("customer", customerId)
            ]
    res ← stripeReq key req POST ["v1","payment_methods",paymentIntentId,"attach"]
    when (isNothing res) $ throwError CE.stripeUser
    runDb $ E.insert_ $ PaymentMethod paymentMethodId entityKey

    pure $ BS8.unpack $ tokenize sk $ BS8.pack paymentMethodId

confirmPaymentIntent
    ∷  ( MonadIO m
       , MonadReader Config m
       , MonadError ServerError m
       )
   ⇒ E.Entity User
   → Token
   → ConfirmPaymentMethod
   → m ()
confirmPaymentIntent
    E.Entity{..}
    token
    ConfirmPaymentMethod{..}
  = do
    let customerId = userCustomerId entityVal
    sk ← asks (^.sk)
    key ← asks (^.booth.stripeKey)
    let paymentMethodId = untokenize sk token

    -- confirm payment intent with payment method
    let req =
            [ ("payment_method", BS8.unpack paymentMethodId)
            , ("setup_future_usage", "off_session")
            ]
    res ← stripeReq key req POST ["v1","payment_intents",paymentIntentId,"confirm"]
    when (isNothing res) $ throwError CE.stripeUser
    pure ()



