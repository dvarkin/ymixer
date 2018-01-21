module Msgs exposing (..)

import RemoteData exposing (WebData)
import Navigation exposing (Location)
import Models exposing (Mix, Channel)

type Msg 
  = OnFetchMixes (WebData (List Mix))
  | OnFetchChannels (WebData (List Channel))
  | OnLocationChange Location


