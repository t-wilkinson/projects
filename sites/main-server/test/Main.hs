module Main where

import Quaalude
import Init (initConfig, shutdown)

-- import           Control.Concurrent.MVar
-- import           Servant.QuickCheck
-- import           Servant.QuickCheck.Internal (serverDoesntSatisfy)
-- import qualified Control.Concurrent               as C
import           Control.Lens              hiding (Context)
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Text                        (Text, unpack)
import           GHC.Generics
import           Network.HTTP.Client       hiding (Proxy)
import           Network.HTTP.Types
import           Network.Wai
import           Prelude ()
import           Prelude.Compat
import           Servant
import           Servant.Client
import           Servant.Server
import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.Matcher
import qualified Data.HashMap.Strict              as HM
import qualified Network.Wai.Handler.Warp         as Warp
import qualified Data.Map.Lazy as Map

import Db
import Web.Cookie
import Api (Route, app)
import Core.App (App, AppT(..), appT, unAppT)
import Core.Config (Client(..), Environment(..), Config(..), ClientName(..), booth, pool)
import Data.Text.Encoding (decodeUtf8)
import Database.Persist.Postgresql (runSqlPool, rawExecute)
import qualified Booth.Api as Booth
import qualified Booth.Api.Payment as Booth
import qualified Booth.Api.Session as Booth
import qualified Core.Errors as CE
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Database.Esqueleto as E
import qualified Database.Persist as P
import qualified Booth.Db as BD

instance ToHttpApiData ByteString where
  toUrlPiece = decodeUtf8

main ∷ IO ()
main = do
    pure ()

-- main ∷ IO ()
-- main = do
--     bracket initConfig shutdown' runTest
--   where
--     runTest cfg = hspec do
--         businessLogicSpec cfg

-- shutdown' ∷ Config → IO ()
-- shutdown' cfg = do
--     shutdown cfg
--     pure ()
--     -- traverse_ dropDb ["\"user\"", "service", "payment_method"]
--     -- where
--     --     dropDb table = runSqlPool (rawExecute ("DROP TABLE " <> table <> " CASCADE") []) (cfg^.booth.pool)

-- withUserApp ∷ Config → (Warp.Port → IO ()) → IO ()
-- withUserApp cfg action =
--     Warp.testWithApplication (pure $ Api.app cfg) action

-- type Api = ToServantApi Api.Route
-- type SClient a b = Servant.Client.Client a b

-- businessLogicSpec ∷ Config → Spec
-- businessLogicSpec cfg =
--   around (withUserApp cfg) $ do

--     baseUrl <- runIO $ parseBaseUrl "http://localhost"
--     manager <- runIO $ newManager defaultManagerSettings
--     user' <- runIO $ runDbBooth $ P.get (fromJust $ decode $ "1")

--     let clientEnv port = mkClientEnv manager (baseUrl { baseUrlPort = port })
--         -- user = P.Entity (fromJust $ decode $ "1") (User "email@email.com" "password"
--         user = fromJust user' :: P.Entity BD.User
--         api = client (Proxy @Api)
--         booth = api
--         (session :<|> service) :<|> (serviceProtected :<|> payment) = booth
--         (login :<|> (register :<|> logout)) :<|> (getUser :<|> getUsers) :<|> (getLoginFieldErrors :<|> changeUser) = session
--         (getServices :<|> getServicesByMonth) = service  undefined -- (undefined :: AuthenticatedRequest (AuthProtect "maybe-booth-user"))
--         addService :<|> (removeService :<|> updateService) = serviceProtected undefined -- (undefined :: AuthenticatedRequest (AuthProtect "booth-user"))
--         (getCharge :<|> (makeCharge :<|> newPaymentIntent)) :<|> confirmPaymentIntent :<|> (addPaymentIntent :<|> getPaymentMethods) = (payment :: _) user "x-fowarded-for"

--         userRegister = Booth.UserRegister "email@email.com" "password" True
--         userLogin = Booth.UserLogin "email@email.com" "password" True
--         wrongUserLogin = Booth.UserLogin "wrong-email@email.com" "password" True
--         sessions = "Right [(\"Set-Cookie\",\"sessions=9489a66b8902c4742a5f398fb5dc3086||1c33dbf78b3eb9107070d68b4461c26dea85b8908ce1dca932515685; Path=/; Max-Age=31536000; HttpOnly; SameSite=Strict\"),(\"Set-Cookie\",\"key=9489a66b8902c4742a5f398fb5dc3086; Path=/; Max-Age=31536000; HttpOnly; SameSite=Strict\")]"
--         run ∷ ∀ a. Int → ClientM a → IO (Either ClientError a)
--         run port client = runClientM client (clientEnv port)
--         true = ()`shouldBe`()
--     session ← runIO $ newIORef Map.empty

--     describe "/booth/session" do
--         it "register" \port → do
--             Right res ← run port $ register "x-fowarded-for" Nothing userRegister
--             let userCookie = lookupResponseHeader res :: ResponseHeader "Set-Cookie" SetCookie
--             liftIO $ modifyIORef session (Map.insert "user-cookie" userCookie)
--             true

--         it "login" \port → do
--             Right res ← run port $ login "x-fowarded-for" Nothing userLogin
--             true

-- --         it "login (wrong email)" \port → do
-- --             res ← run port $ login "x-fowarded-for" Nothing wrongUserLogin
-- --             show (show res) `shouldBe` "\"Left (FailureResponse (Request {requestPath = (BaseUrl {baseUrlScheme = Http, baseUrlHost = \\\"localhost\\\", baseUrlPort = 35691, baseUrlPath = \\\"\\\"},\\\"/booth/session/login\\\"), requestQueryString = fromList [], requestBody = Just ((),application/json;charset=utf-8), requestAccept = fromList [application/json;charset=utf-8,application/json], requestHeaders = fromList [(\\\"X-Forwarded-For\\\",\\\"x-fowarded-for\\\")], requestHttpVersion = HTTP/1.1, requestMethod = \\\"POST\\\"}) (Response {responseStatusCode = Status {statusCode = 401, statusMessage = \\\"Unauthorized\\\"}, responseHeaders = fromList [(\\\"Transfer-Encoding\\\",\\\"chunked\\\"),(\\\"Date\\\",\\\"Wed, 26 Aug 2020 13:15:40 GMT\\\"),(\\\"Server\\\",\\\"Warp/3.3.12\\\")], responseHttpVersion = HTTP/1.1, responseBody = \\\"Invalid credentials.\\\"}))\""

-- --         it "Logout" \ → do
-- --             res ← run $ Booth.logout
-- --             show res `shouldBe` "Right [(\"Set-Cookie\",\"key=; Path=/; HttpOnly; SameSite=Strict\")]"

--     describe "/booth/payment" do
--         it "getCharge" \port → do
--             res ← run port $ getCharge
--             print res
--             true


-- -- instance (GetHeaders (HList ls), Eq a, Show a) ⇒ Eq (Headers ls a) where
-- --   h1 == h2 = getResponse h1 == getResponse h2
-- --         && on (==) (getHeaders . getHeadersHList) h1 h2

-- instance (GetHeaders (HList ls)) ⇒ Show (Headers ls a) where
--    show = show . getHeaders . getHeadersHList

-- -- instance (KnownSymbol l) => Show (HList (Servant.Header l x ': ls)) where
-- --     show (h `HCons` hs) = show h <> show hs
-- --     show HNil = ""

-- -- instance (Show a) => Show (Headers ls a) where
-- --     show (Headers res hs) = show hs <> show res
