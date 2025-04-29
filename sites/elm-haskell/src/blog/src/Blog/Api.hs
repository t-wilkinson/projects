module Blog.Api
     ( API
     , server
     , database
     ) where

-- General
import GHC.Generics
import Control.Monad.IO.Class (liftIO)
import Data.Default (def)
import Data.SafeCopy
import Control.Applicative

-- API
import Servant.API 
import Servant 

-- Network
import Network.Wai
import Network.Wai.Handler.Warp ( run )
import Network.Wai.Middleware.Cors 
import Network.Wai.Middleware.RequestLogger ( logStdoutDev )
import Network.Wai.Middleware.Servant.Options
import Network.HTTP.Types  ( status404, status200 )

-- Database
import Blog.Database hiding (addBlog, addComment, deleteBlog, deleteBlogs, deleteComment, deleteUsers, editBlog, getBlog, getBlogTitle, getBlogs, getUser, getUsers, insertBlog, login, register)
import Data.IntMap (IntMap, Key)
import Data.Typeable
import qualified Data.IntMap as IntMap
import Data.Acid 



---- Api ----


type API = 
           "user" :> 
          (    Get '[JSON] [User]
          :<|> Delete '[JSON] ()
          )
      :<|> "login" :> ReqBody '[JSON] User :> Post '[JSON] String
      :<|> "register" :> ReqBody '[JSON] User :> Post '[JSON] String
      :<|> "blog" :> 
          (    Get '[JSON] [Blog]
          :<|> Delete '[JSON] ()
          :<|> Capture "title" String :> Delete '[JSON] ()
          :<|> Capture "title" String :> Get '[JSON] (Maybe Blog)
          :<|> ReqBody '[JSON] Blog :> Post '[JSON] String
          )
      :<|> "comment" :>
          (    Capture "title" String :> ReqBody '[JSON] Comment :> Post '[JSON] ()
          :<|> Capture "title" String :> ReqBody '[JSON] Comment :> Delete '[JSON] ()
          )



---- Server ----


server :: DB -> Server API
server (blogdb,userdb) =
    users userdb :<|> login userdb :<|> register userdb :<|> blogs blogdb :<|> comments blogdb
  where
    users userdb = getUsers userdb 
              :<|> deleteUsers userdb

    blogs blogdb = getBlogs blogdb 
              :<|> deleteBlogs blogdb 
              :<|> deleteBlog blogdb
              :<|> getBlogTitle blogdb 
              :<|> postBlog blogdb

    comments blogdb = addComment blogdb
                 :<|> deleteComment blogdb


-- Users


--query' :: (QueryEvent event) => AcidState (EventState event) -> event -> Handler (EventResult event)


--update' :: (UpdateEvent event) => AcidState (EventState event) -> event -> Handler (EventResult event)
query' db event = liftIO $ query db event
update' db event = liftIO $ update db event


getUsers :: AUsers -> Handler [User]
getUsers db = query' db (GetUsers 10) 


deleteUsers :: AUsers -> Handler ()
deleteUsers db = do
    update' db DeleteUsers
    return ()


login :: AUsers -> User -> Handler String
login db user = query' db (Login user)


register :: AUsers -> User -> Handler String
register db user = update' db (Register user)


-- Blogs

getBlogs :: ABlogs -> Handler [Blog]
getBlogs db = liftIO $ query db (GetBlogs 10) 

deleteBlogs :: ABlogs -> Handler ()
deleteBlogs db = do
    _   <- liftIO $ update db DeleteBlogs
    return ()

deleteBlog :: ABlogs -> String -> Handler ()
deleteBlog db title = do
    _ <- liftIO $ update db (DeleteBlog title)
    return ()

getBlogTitle :: ABlogs -> String -> Handler (Maybe Blog)
getBlogTitle db i = liftIO $ query db (GetBlogTitle i)

postBlog :: ABlogs -> Blog -> Handler String
postBlog db blog = do
    _ <- liftIO $ update db (AddBlog blog)
    return "success"


-- Comments

addComment :: ABlogs -> String -> Comment -> Handler ()
addComment db title comment = do
    _ <- liftIO $ update db (AddComment title comment )
    return ()

deleteComment :: ABlogs -> String -> Comment -> Handler ()
deleteComment db title comment = do
    _ <- liftIO $ update db (DeleteComment title comment)
    return ()



---- Database ----


database :: IO DB
database =
    liftA2 (,)  
        (openDatabase "blog-posts" (Blogs []))
        (openDatabase "users" (Users [User "admin" "admin"]))

    where
        openDatabase location = openLocalStateFrom ("../database/" ++ location)

