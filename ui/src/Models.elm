module Models exposing (..)

import RemoteData exposing (WebData)
import Material


type Route 
  = MixesRoute
  | MixRoute MixId
  | NotFoundRoute


type alias Mdl =
  Material.Model


type alias Model =
  { mixes : WebData (List Mix)
  , mix : Maybe MixId
  , channels : WebData (List Channel)
  , route : Route
  , mdl : Mdl
  }


initialModel : Route -> Maybe MixId -> Model 
initialModel route mix = 
  { mixes = RemoteData.Loading
  , mix = mix
  , channels = RemoteData.NotAsked
  , route = route
  , mdl = Material.model
  }


type alias ImageUrl =
  String


type alias ChannelId =
  Int


--type ChannelStatus 
--  = On 
--  | Off


type alias ChannelName =
  String 


type alias Channel =
  { id : ChannelId
  , name : ChannelName
  , on : Bool
  , image : ImageUrl
  }


type alias MixId =
  Int 


type alias MixName =
  String 


type alias Mix =
  { id : MixId 
  , name : String
  }

