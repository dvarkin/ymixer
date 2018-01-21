module Models exposing (..)

import RemoteData exposing (WebData)


type Route 
  = MixesRoute
  | MixRoute MixId
  | NotFoundRoute


type alias Model =
  { mixes : WebData (List Mix)
  ,  route : Route
  }


initialModel : Route -> Model 
initialModel route = 
  { mixes = RemoteData.Loading
  , route = route
  }


type alias ImageUrl =
  String


type alias ChannelId =
  Int


type ChannelStatus 
  = On 
  | Off


type alias ChannelName =
  String 


type alias Channel =
  { id : ChannelId
  , name : ChannelName
  , status : ChannelStatus
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

