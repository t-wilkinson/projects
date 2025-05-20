module Booth.Api.ServiceProtected where

import Quaalude
import Booth.Service (UpdateService, AddService, addService, deleteService, updateService, setServiceProcessing)
import Core.App (AppS)
import Core.Token (SToken)
import Booth.Db
import qualified Database.Esqueleto as E

data Route r = Route
    { _addService ∷ r
        :- ReqBody '[JSON] AddService
        :> Post '[JSON] (Key Service)
    , _deleteService ∷ r
        :- QueryParam' '[Strict, Required] "token" SToken
        :> Delete '[JSON] ()
    , _updateService ∷ r
        :- Capture "token" SToken
        :> ReqBody '[JSON] UpdateService
        :> Put '[JSON] ()
    , _setServiceProcessing ∷ r
        :- "processing"
        :> Capture "token" SToken
        :> Put '[JSON] ()
    } deriving (Generic)

route ∷ E.Entity User → Route AppS
route user = Route
    (addService user)
    (deleteService user)
    (updateService user)
    (setServiceProcessing user)
