module Msgs exposing (..)

import RemoteData exposing (WebData)
import Navigation exposing (Location)
import Models exposing (Mix)

type Msg 
  = OnFetchMixes (WebData (List Mix))
  | OnLocationChange Location


