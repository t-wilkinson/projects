data SquareAmount = SquareAmount
    { amount ∷ Int
    , currency ∷ String
    } deriving (Generic, ToJSON, FromJSON)

data SquarePayment = SquarePayment
    { amount_money ∷ SquareAmount
    , idempotency_key ∷ String
    , source_id ∷ String
    } deriving (Generic, ToJSON, FromJSON)

data SquareConfirm = SquareConfirm
    { nonce ∷ String
    , uuid ∷ String
    } deriving (Generic, ToJSON, FromJSON)

confirm
    :: ∀ m.
        ( MonadIO m
        , MonadReader Config m
        , MonadError ServerError m
        )
    ⇒ E.Entity User
    → SquareConfirm
    → m ()
confirm entityUser (SquareConfirm nonce uuid) = do
    sk ← asks (^.sk)
    key ← asks (^.booth.squareKey)
    charge ← getUserCharge entityUser
    let squareAmount = (SquareAmount (floor charge) "USD")
        squarePayment = (SquarePayment squareAmount uuid nonce)
    res ← squareReq key (encode squarePayment) POST ["v2","payments"]
    case mayFromJSON $ res ^? JSON.key "errors" of
        Just (errors ∷ [String]) → throwError CE.squareUser
        Nothing → pure ()


parseSquareRes ∷ (MonadError ServerError m, FromJSON a) ⇒ Maybe Value → m a
parseSquareRes mayValue = maybe (throwError CE.squareUser) (pure) $ mayFromJSON mayValue

{- Square Requests -}
squareReq
    ∷ (MonadReader Config m, MonadIO m)
    ⇒ ServerKey → BSL8.ByteString → StdMethod → [String]
    → m BSL8.ByteString
squareReq key body method paths = do
    manager ← asks (^.manager)
    env ← asks (^.env)
    let url = case env of
            Development → "https://connect.squareupsandbox.com"
            Production → "https://connect.squareup.com"
    liftIO $ fmap (responseBody) $ squareReq' url key body method paths manager

squareReq'
    ∷ Request
    → ServerKey → BSL8.ByteString → StdMethod → [String]
    → Manager → IO (Response BSL8.ByteString)
squareReq' origin key body method paths manager =
    (httpLbs origin
        { method = BS8.pack $ show method
        , path = BS8.pack $ intercalate "/" paths
        , requestHeaders = headers
        , requestBody = RequestBodyLBS body
        } manager)
  where
    headers =
        [ ("Content-Type", "application/json")
        , ("Authorization", "Bearer " <> "$SQUARE_ACCESS_TOKEN")
        , ("Square-Version", "2020-09-23")
        ]

