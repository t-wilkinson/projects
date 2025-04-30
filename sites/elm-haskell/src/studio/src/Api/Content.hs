-- |

module Api.Content where

import Config ( App )
import Db ( runDb, toVal )
import Db.Content

import Database.Persist
import Control.Exception ( throw )
import Servant (err404)


data Route route = Route
  { _getAllContent :: route
    :- Get '[JSON] [Content]

  , _getContentByHeading :: route
    :- Capture "heading" String
    :> Get '[JSON] Content

  , _delAllContent :: route
    :- Delete '[JSON] ()

  , _delContentByHeading :: route
    :- Capture "heading" String
    :> Delete '[JSON] ()

  , _putContentByHeading :: route
    :- Capture "heading" String
    :> ReqBody '[JSON] Content
    :> Put '[JSON] ()

  , _postContent :: route
    :- ReqBody '[JSON] Content
    :> Post '[JSON] ()
  } deriving Generic

-- type API = ToServant Route AsApi
type Heading = String

route = Route @(AsServerT App)
    getAllContent
    getContentByHeading
    delAllContent
    delContentByHeading
    putContentByHeading
    postContent

getAllContent :: App [Content]
getAllContent = toVal . runDb $ selectList [] []

getContentByHeading :: Heading -> App Content
getContentByHeading h = runDb $ selectFirst [ContentHeading ==. h] [] >>= \case
  Just (Entity _ v) -> pure v
  Nothing             -> throw err404

delAllContent :: App ()
delAllContent = do
  runDb $ deleteWhere [ContentHeading !=. ""]
  pure ()

delContentByHeading :: Heading -> App ()
delContentByHeading h = do
  runDb $ deleteWhere [ContentHeading ==. h]
  pure ()

putContentByHeading :: Heading -> Content -> App ()
putContentByHeading h c = runDb $ selectFirst [ContentHeading ==. h] [] >>= \case
  Just (Entity key _) -> pure (runDb $ replace key c) >> pure ()
  Nothing             -> throw err404

postContent :: Content -> App ()
postContent c = do
  liftIO $ print c
  runDb $ insert_ c
  pure ()
