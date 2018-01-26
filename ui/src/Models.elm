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
  , cardSize : Int 
  , editPhotos : Bool
  }


initialModel : Route -> Maybe MixId -> Model 
initialModel route mix = 
  { mixes = RemoteData.Loading
  , mix = mix
  , channels = RemoteData.NotAsked
  , route = route
  , mdl = Material.model
  , cardSize = 150
  , editPhotos = False
  }


type alias ImageUrl =
  String


type alias ChannelId =
  Int


type alias ChannelName =
  String 


type alias Channel =
  { id : ChannelId
  , on : Bool
  , image : ImageUrl
  }


type alias MixId =
  Int 


type alias Mix =
  { id : MixId 
  }

