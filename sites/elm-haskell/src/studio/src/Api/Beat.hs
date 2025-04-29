-- | Api for beats

module Api.Beat where

import Config ( App, Config )
import Db ( runDb, toVal )
import Db.Beat

import Servant ( err404 )
import Database.Persist ( Entity(Entity)
                        , (==.)
                        , selectFirst , selectList , insert , replace )
import Control.Exception ( throw )


data Route route = Route
  { _getBeatByTitle :: route
    :- Capture "title" String
    :> Get '[JSON] [Beat]

  , _delBeatByTitle :: route
    :- Capture "title" String
    :> Delete '[JSON] ()

  , _putBeatByTitle :: route
    :- Capture "title" String
    :> ReqBody '[JSON] Beat
    :> Put '[JSON] ()

  , _postBeat :: route
    :- ReqBody '[JSON] Beat
    :> Post '[JSON] ()
  } deriving Generic

type BeatTitle = String


route = Route @(AsServerT App)
    getBeatByTitle
    delBeatByTitle
    putBeatByTitle
    postBeat

getBeatByTitle :: BeatTitle -> App [Beat]
getBeatByTitle n = toVal . runDb $ selectList [BeatName ==. n] []

delBeatByTitle :: BeatTitle -> App ()
delBeatByTitle n = return ()

putBeatByTitle :: BeatTitle -> Beat -> App ()
putBeatByTitle n b = runDb $ selectFirst [BeatName ==. n] [] >>= \case
  Just (Entity key _) -> pure (runDb $ replace key b) >> pure ()
  Nothing             -> throw err404

postBeat :: Beat -> App ()
postBeat b = do
  runDb (insert b)
  return ()
