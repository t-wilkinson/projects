module Blog.Gen 
     ( generate
     ) where


import Blog.Api ( API )
import Blog.Database 
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
    ["Generated", "API"]
    defElmImports
    "client/src"
    [ DefineElm (Proxy :: Proxy Blog)
    , DefineElm (Proxy :: Proxy User)
    , DefineElm (Proxy :: Proxy Comment)
    ]
    (Proxy :: Proxy API)


