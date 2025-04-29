module Session exposing (Session, toKey)

import Browser.Navigation as Nav


type alias Session =
    { nav : Nav.Key }


toKey : Session -> Nav.Key
toKey session =
    session.nav
