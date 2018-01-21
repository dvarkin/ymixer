module Msgs exposing (..)

import RemoteData exposing (WebData)
import Navigation exposing (Location)
import Models exposing (Mix, Channel, ChannelId)
import Material 


type Msg 
  = OnFetchMixes (WebData (List Mix))
  | OnFetchChannels (WebData (List Channel))
  | OnLocationChange Location
  | Mdl (Material.Msg Msg)
  | NewUrl String
  | SetChannel ( ChannelId, Bool )


