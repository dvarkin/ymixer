module Models exposing (..)

import RemoteData exposing (WebData)


type alias Model =
  { mixes : WebData (List Mix)
  }


initialModel : Model 
initialModel =
  { mixes = RemoteData.Loading
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

