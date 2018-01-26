module Msgs exposing (..)

import RemoteData exposing (WebData)
import Navigation exposing (Location)
import Models exposing (MixId, Channel, ChannelId, MixId)
import Material 
import Keyboard
import Http

type Msg 
  = Mdl (Material.Msg Msg)
  | KeyMsg Keyboard.KeyCode
  | NavigateToUrl String
  | EditPhotos
  | OnLocationChange Location
  | OnFetchChannels (WebData (List Channel))
  | OnFetchMixes (WebData (List MixId))
  | MuteMix MixId
  | OnMuteMix (Result Http.Error ())
  | SetChannel ( MixId, ChannelId, Bool )
  | OnSetChannel (Result Http.Error (MixId, ChannelId, Bool))

