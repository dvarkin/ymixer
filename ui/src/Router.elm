module Router exposing (..)

import Navigation exposing (Location)
import Models exposing (MixId, Route(..))
import UrlParser exposing (..) 
import Msgs exposing (Msg)


matchers : Parser (Route -> a) a
matchers =
  oneOf
    [ map MixesRoute top    
    , map MixRoute (s "mixes" </> int)
    , map MixesRoute (s "mixes")
    ]


parseLocation : Location -> Route 
parseLocation location =
  case (parseHash matchers location) of
    Just route -> 
      route 

    Nothing ->
      NotFoundRoute


mixesPath : String
mixesPath =
  "#mixes"


mixPath : MixId -> String
mixPath id =
  mixesPath ++ "/" ++ (toString id)


gotoMix : MixId -> Msg
gotoMix id =
  Msgs.NavigateToUrl (mixPath id)


gotoMixes : Msg
gotoMixes =
  Msgs.NavigateToUrl mixesPath

