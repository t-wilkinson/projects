{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Main where

import Core.Config (Environment(..), env)
import Init (initConfig, shutdown, runApp)
import Quaalude
import qualified Booth.Api
import qualified Dogwalking.Api

import Servant.JS                  (CommonGeneratorOptions(..), defCommonGeneratorOptions, jsForAPI, vanillaJSWith)
import Text.Regex                  (subRegex, mkRegex)
import qualified Data.Text as T
import Servant.Foreign (HasForeignType, HasForeign, Foreign, foreignFor)
import Servant.API.Modifiers (RequiredArgument)

instance {-# OVERLAPPING #-} (HasForeignType lang ftype (RequiredArgument mods a), HasForeign lang ftype api)
  => HasForeign lang ftype (Header' mods "Cookie" a :> api) where
  type Foreign ftype (Header' mods "Cookie" a :> api) = Foreign ftype api
  foreignFor lang ftype Proxy req =
    foreignFor lang ftype (Proxy :: Proxy api) req

instance {-# OVERLAPPING #-} (HasForeignType lang ftype (RequiredArgument mods a), HasForeign lang ftype api)
  => HasForeign lang ftype (Header' mods "cookie" a :> api) where
  type Foreign ftype (Header' mods "cookie" a :> api) = Foreign ftype api
  foreignFor lang ftype Proxy req =
    foreignFor lang ftype (Proxy :: Proxy api) req

-- | Run our Server
main ∷ IO ()
main = do
    cfg ← initConfig
    case cfg ^. env of
        Development → writeApiToJS
        Remote → pure ()
        Production → pure ()
    bracket (pure cfg) shutdown runApp

writeApiToJS ∷ IO ()
writeApiToJS = do
    writeFile "../nyah.w-dogwalking/src/api.tsx" $  dogwalkingApi
    writeFile "../max.k-studio/src/api.tsx" $ boothApi
  where
    dogwalkingApi = toJSApi Dogwalking.Api.api "https://api.treywilkinson.com/dogwalking"
    boothApi = toJSApi Booth.Api.api "https://api.kleanstudio.com/booth"

toJSApi api prefix = "/* eslint-disable */\n" <> apiForJS
  where
    -- options = defCommonGeneratorOptions { urlPrefix = "https://api.treywilkinson.xyz/" <> account }
    options = defCommonGeneratorOptions { urlPrefix = prefix }
    apiForJS' = T.unpack $ jsForAPI api (vanillaJSWith options)
    apiForJS'' = subRegex (mkRegex "^var") apiForJS' "export var"
    apiForJS''' = subRegex (mkRegex "^(  xhr.onreadystatechange.*)") apiForJS'' "  xhr.withCredentials = true;\n\\1"
    apiForJS = subRegex (mkRegex "if \\(res\\)") apiForJS''' "if (res !== undefined && res !== null) "

