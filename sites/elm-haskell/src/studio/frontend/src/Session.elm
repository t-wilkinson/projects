module Session exposing (Error, FormError(..), Session, Status(..), User(..), isLoggedIn, login, logout)

import Browser.Navigation as Nav
import Html
import Http


type alias Error e =
    ( e, String )


type FormError e
    = FieldError (Error e)
    | ServerError String


type alias Field form =
    { setter : form -> String -> form
    , id : String
    }


type Status a
    = Failed Http.Error
    | Loading
    | Recieved a



--- Session ---


type alias Session =
    { navKey : Nav.Key
    , user : User
    }


type User
    = LoggedIn String
    | Guest


type alias Username =
    String


login : Session -> Username -> Session
login session username =
    { session | user = LoggedIn username }


logout : Session -> Session
logout session =
    { session | user = Guest }


isLoggedIn : Session -> Bool
isLoggedIn { user } =
    case user of
        Guest ->
            False

        _ ->
            True
