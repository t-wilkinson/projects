module Elders.Gen 
     ( generate
     ) where


import Elders.Api ( API )
import Elders.Database 
import Servant.Elm ( DefineElm(DefineElm)
                   , Proxy(Proxy)
                   , defElmImports
                   , generateElmModuleWith
                   , ElmOptions
                   , defElmOptions
                   , urlPrefix
                   , UrlPrefix(..)
                   )


generate :: IO ()
generate = putStrLn "Generating..." *> generateElmModuleWith
    (defElmOptions { urlPrefix = Static "https://treywilkinson.xyz:8080" })
    ["Api"]
    defElmImports
    "client/src"
    [ DefineElm (Proxy :: Proxy People)
    , DefineElm (Proxy :: Proxy Days)
    , DefineElm (Proxy :: Proxy Person)
    , DefineElm (Proxy :: Proxy Day)
    ]
    (Proxy :: Proxy API)



