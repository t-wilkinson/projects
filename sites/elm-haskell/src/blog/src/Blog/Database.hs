module Blog.Database where 


import GHC.Generics
import Elm.Derive ( defaultOptions, deriveBoth )
import Control.Lens (makeLenses)
import Control.Monad.Reader (ask)
import qualified Control.Monad.State as S

import Data.List
import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Acid
import Data.SafeCopy
import Data.IntMap (IntMap, Key)
import Data.Typeable
import Data.Default
import qualified Data.IntMap as IntMap
import qualified Data.Map as M
import qualified Data.Map as Map




---- Types ----


type DB = (ABlogs, AUsers)


type AUsers = AcidState Users
type ABlogs = AcidState Blogs


data Comment = Comment
    { commentUser :: String
    , commentContent :: String
    } deriving (Show, Eq, Ord, Generic)


data Blog = Blog
    { user :: String
    , title :: String
    , content :: String
    , comments :: [Comment]
    } deriving (Show, Eq, Ord, Generic)


data User = User 
    { username :: String
    , password :: String
    } deriving (Show, Eq, Ord, Generic)

newtype Users = Users [User]
    deriving (Show, Ord, Eq, Typeable, Generic)
instance ToJSON Users


newtype Blogs = Blogs [Blog]
    deriving (Show, Ord, Eq, Typeable, Generic)
instance ToJSON Blogs


deriveBoth defaultOptions ''User
deriveBoth defaultOptions ''Blog
deriveBoth defaultOptions ''Comment


$(deriveSafeCopy 0 'base ''Blog)
$(deriveSafeCopy 0 'base ''Blogs)
$(deriveSafeCopy 0 'base ''User)
$(deriveSafeCopy 0 'base ''Users)
$(deriveSafeCopy 0 'base ''Comment)
makeLenses ''Blogs
makeLenses ''Users



---- Blogs ----


-- map over blogs
-- if blog titles are equal, update contents
-- otherwise prepend
-- predicate all blogs with matching titles
-- postpend number if duplicate title
-- most likely much easier solution
addBlog :: Blog  -> Update Blogs ()
addBlog blog = do 
    Blogs bs <- S.get
    S.put $ Blogs $ 
        if any (\b -> title b == title blog) bs
            then update bs
            else blog { title = title blog ++ prepend bs}:bs
  where
    update = map (\b -> if title b == title blog then blog { user = user b } else b) 
    prepend = format . show . length . filter (\b -> title' b == title blog) 
    title' b = take ((+(-2)) . length . title $ b) (title b)
    format xs
      | length xs < 2 = format ('0':xs)
      | otherwise = xs

editBlog :: Blog -> Update Blogs ()
editBlog blog = do
    Blogs bs <- S.get
    S.put $ Blogs $ map (\b -> if title b == title blog then blog else b) bs

-- return blog whose title matches string
getBlogTitle :: String -> Query Blogs (Maybe Blog)
getBlogTitle t = do
    Blogs bs <- ask
    return $ find (\blog -> title blog == t) bs


getBlogs :: Int -> Query Blogs [Blog]
getBlogs limit = do 
    Blogs bs <- ask
    return $ take limit bs


deleteBlogs :: Update Blogs ()
deleteBlogs = S.put $ Blogs []

deleteBlog :: String -> Update Blogs ()
deleteBlog t = do
    Blogs bs <- S.get
    S.put $ Blogs $ filter (\blog -> title blog /= t) bs

addComment :: String -> Comment -> Update Blogs ()
addComment t c = do
    Blogs bs <- S.get
    S.put $ Blogs $ map appendComment bs
  where
    appendComment blog@(Blog _ title _ cs) = 
        if title == t 
           then blog {comments = c:cs} 
           else blog

deleteComment :: String -> Comment -> Update Blogs ()
deleteComment t comment = do
    Blogs bs <- S.get
    S.put $ Blogs $ map remove bs
  where
    remove blog = 
        if title blog /= t 
            then blog
            else blog { comments = filter (comment /=) . comments $ blog}


---- Users ----


register :: User -> Update Users String
register user = do 
    Users us <- S.get

    if any (\u -> username u == username user) us
        then return "Username is already in use" 
        else do
            S.put $ Users $ user:us
            return "success"



getUsers :: Int -> Query Users [User]
getUsers limit = do 
    Users us <- ask
    return us


deleteUsers :: Update Users ()
deleteUsers = S.put (Users [])


login :: User -> Query Users String
login user = do
    Users u <- ask
    return $ if user `elem` u then "success" else "Username/Password is wrong"



$(makeAcidic ''Users [ 'register
                     , 'getUsers
                     , 'deleteUsers
                     , 'login
                     ])

$(makeAcidic ''Blogs [ 'addBlog
                     , 'getBlogs
                     , 'deleteBlogs
                     , 'getBlogTitle
                     , 'addComment
                     , 'deleteComment
                     , 'deleteBlog
                     , 'editBlog
                     ])



