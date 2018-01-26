module Msgs exposing (..)

import RemoteData exposing (WebData)
import Navigation exposing (Location)
import Models exposing (MixId, Channel, ChannelId, MixId)
import Material 
import Keyboard
import Http

type Msg 
  = OnFetchMixes (WebData (List MixId))
  | OnFetchChannels (WebData (List Channel))
  | OnLocationChange Location
  | TurnMixOff MixId
  | Mdl (Material.Msg Msg)
  | NewUrl String
  | KeyMsg Keyboard.KeyCode
  | EditPhotos
  | OnMixTurnOff (Result Http.Error String)
  | SetChannel ( MixId, ChannelId, Bool )
  | OnSetChannel (Result Http.Error (MixId, ChannelId, Bool))

