module Commands exposing (..)

import Http
import Json.Decode as Decode 
import Json.Decode.Pipeline exposing (decode, required)
import RemoteData
import Models exposing (Mix, MixId, Channel)
import Msgs exposing (..)


fetchMixes : Cmd Msg 
fetchMixes =
  Http.get fetchMixesUrl mixesDecoder
    |> RemoteData.sendRequest
    |> Cmd.map OnFetchMixes


fetchMixesUrl : String
fetchMixesUrl =
  "/api/mixes"


mixesDecoder : Decode.Decoder (List Mix)
mixesDecoder =
  Decode.list mixDecoder 


mixDecoder : Decode.Decoder Mix 
mixDecoder =
  decode Mix
    |> required "id" Decode.int
    |> required "name" Decode.string 


fetchChannels : MixId -> Cmd Msg
fetchChannels id =
  Http.get (fetchChannelsUrl id) channelsDecoder
    |> RemoteData.sendRequest
    |> Cmd.map OnFetchChannels


fetchChannelsUrl : MixId -> String 
fetchChannelsUrl id =
  "/api/mixes/" ++ (toString id)


channelsDecoder : Decode.Decoder (List Channel)
channelsDecoder =
  Decode.list channelDecoder 


channelDecoder : Decode.Decoder Channel
channelDecoder =
  decode Channel 
    |> required "id" Decode.int
    |> required "on" Decode.bool
    |> required "image" Decode.string





