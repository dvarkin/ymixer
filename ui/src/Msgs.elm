module Msgs exposing (..)

import RemoteData exposing (WebData)
import Navigation exposing (Location)
import Models exposing (MixId, Channel, ChannelId)
import Material 
import Keyboard

type Msg 
  = OnFetchMixes (WebData (List MixId))
  | OnFetchChannels (WebData (List Channel))
  | OnLocationChange Location
  | Mdl (Material.Msg Msg)
  | NewUrl String
  | SetChannel ( ChannelId, Bool )
  | ChangeCardSize Int
  | KeyMsg Keyboard.KeyCode
  | SelectTab Int


