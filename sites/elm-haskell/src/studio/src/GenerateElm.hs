{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}

-- |

module GenerateElm where

import Api ( Api )
import Config ( Config, Environment(..), env, port )
import Db
import Db.User
import Db.Content
import Db.Beat
import Db.Studio

import Data.Dynamic
import Data.Typeable (typeOf, TypeRep)
import           Servant.Elm                    ( DefineElm(DefineElm)
                                                , Proxy(Proxy)
                                                , defElmImports
                                                , generateElmModuleWith
                                                , defElmOptions
                                                , urlPrefix
                                                , UrlPrefix(..)
                                                )
import Data.Text ( pack )


defineElm :: [DefineElm]
defineElm = [ DefineElm (Proxy @Beat)
            , DefineElm (Proxy @Tag)
            , DefineElm (Proxy @Content)
            , DefineElm (Proxy @User)
            , DefineElm (Proxy @Login)
            , DefineElm (Proxy @Register)
            , DefineElm (Proxy @StudioRequest)
            ]

elmGen :: Config -> IO ()
elmGen cfg = do
  putStrLn "Generating..."
  generateElmModuleWith
    (defElmOptions
      { urlPrefix = Static
                      $ toServerUrl (cfg ^. env) (pack . show $ view port cfg)
      }
    )
    ["Api"]
    defElmImports
    "frontend/src"
    defineElm
   -- create haskell template to create elm instances and add them here
    (Proxy @Api)

toServerUrl :: Environment -> Text -> Text
toServerUrl Development = (<>) "http://treywilkinson.xyz:"
toServerUrl Production  = (<>) "https://treywilkinson.xyz:"
toServerUrl Test        = (<>) "http://localhost:"

