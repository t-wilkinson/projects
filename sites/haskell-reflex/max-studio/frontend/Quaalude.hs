module Quaalude
    ( module P
    , module X
    , module Reflex
    , module Reflex.Dom
    , (!)
    , btn
    , nil
    , type View
    ) where

import Prelude as P
import Reflex hiding (Home, askEvents)
import Reflex.Dom hiding (Home, askEvents)
import qualified Control.Lens as X ((^.), (.~))
import Data.Text as X (Text, pack)
import Data.Map as X (Map)
import Data.List as X
import Data.Maybe as X (fromMaybe)

type View = forall t m. MonadWidget t m => m ()

nil :: Map Text Text
nil = mempty

infixr 1 !
(!) :: MonadWidget t m => Text -> Map Text Text -> (m a -> m a)
(!) = elAttr

btn :: forall t m a. MonadWidget t m => Map Text Text -> m a -> m (Event t ())
btn attrs ma = domEvent Click . fst <$> (elAttr' "button" attrs $ ma)
