module Commands exposing (..)

import Http
import Json.Decode as Decode 
import Json.Decode.Pipeline exposing (decode, required)
import RemoteData
import Models exposing (Mix, MixId, Channel)
import Msgs exposing (..)


fetchMixes : Cmd Msg 
fetchMixes =
  Http.request
    { method = "GET"
    , headers = [ Http.header "Content-type" "application/json" ]
    , url = fetchMixesUrl
    , body = Http.emptyBody
    , expect = Http.expectJson mixesDecoder
    , timeout = Nothing
    , withCredentials = False
    }
    |> RemoteData.sendRequest
    |> Cmd.map OnFetchMixes


fetchMixesUrl : String
fetchMixesUrl =
  "/api/mixes"


mixesDecoder : Decode.Decoder (List MixId)
mixesDecoder =
  Decode.list Decode.int


fetchChannels : MixId -> Cmd Msg
fetchChannels id =
  Http.request
    { method = "GET"
    , headers = [ Http.header "Content-type" "application/json" ]
    , url = fetchChannelsUrl id
    , body = Http.emptyBody
    , expect = Http.expectJson channelsDecoder
    , timeout = Nothing
    , withCredentials = False
    }
    |> RemoteData.sendRequest
    |> Cmd.map OnFetchChannels


fetchChannelsUrl : MixId -> String 
fetchChannelsUrl id =
  "/api/mix/" ++ (toString id)


channelsDecoder : Decode.Decoder (List Channel)
channelsDecoder =
  Decode.list channelDecoder 


channelDecoder : Decode.Decoder Channel
channelDecoder =
  decode Channel 
    |> required "id" Decode.int
    |> required "on" Decode.bool
    |> required "image" Decode.string





