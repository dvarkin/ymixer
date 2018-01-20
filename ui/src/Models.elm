module Models exposing (..)


type alias Model =
  String


initialModel : Model 
initialModel =
  "initial model"


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


type alias Channels =
  List Channel 


type alias MixId =
  Int 


type alias MixName =
  String 


type alias Mix =
  { id : MixId 
  , name : String
  , channels : Channels
  }


type alias Mixes =
  List Mix 

