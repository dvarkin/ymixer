module Msgs exposing (..)

import RemoteData exposing (WebData)
import Models exposing (Mix)

type Msg = 
  OnFetchMixes (WebData (List Mix))

