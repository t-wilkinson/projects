module Api.Studio where

import Config ( App )
import Db ( runDb, toVal )
import Db.Studio ( StudioRequest(..) )

import Control.Exception ( throw )
import Servant ( err404 )
import Database.Persist ( Entity(Entity)
                                , (==.)
                                , insert )


data Route route = Route
    { _postStudioRequest :: route
        :- ReqBody '[JSON] StudioRequest
        :> Post '[JSON] ()
    } deriving Generic


route = Route @(AsServerT App)
    postStudioRequest


postStudioRequest :: StudioRequest -> App ()
postStudioRequest studioRequest = do
    runDb (insert studioRequest)
    return ()


