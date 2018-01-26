module Commands exposing (..)

import Http
import Json.Decode as Decode 
import Json.Decode.Pipeline exposing (decode, required)
import RemoteData
import Models exposing (Mix, MixId, Channel, ChannelId)
import Msgs exposing (..)


setChannel : MixId -> ChannelId -> Bool -> Cmd Msg 
setChannel mix chan on =
  let
    urlFn =
      if on then
        turnOnChanUrl
      else
        turnOffChanUrl
  in
    Http.request
      { method = "POST"
      , headers = [ Http.header "Content-type" "application/json" ]
      , url = urlFn mix chan
      , body = Http.emptyBody
      , expect = Http.expectStringResponse (\_ -> Ok ( mix, chan, on ))
      , timeout = Nothing
      , withCredentials = False
      } 
      |> Http.send OnSetChannel


turnOnChanUrl : MixId -> ChannelId -> String
turnOnChanUrl mix chan =
  "/api/channel/switch/" ++ toString mix ++ "/" ++ toString chan ++ "/on"


turnOffChanUrl : MixId -> ChannelId -> String
turnOffChanUrl mix chan =
  "/api/channel/switch/" ++ toString mix ++ "/" ++ toString chan ++ "/off"


turnMixOff : MixId -> Cmd Msg 
turnMixOff id =
  Http.request
    { method = "POST"
    , headers = [ Http.header "Content-type" "application/json" ]
    , url = fetchChannelsUrl id
    , body = Http.emptyBody
    , expect = Http.expectStringResponse (\_ -> Ok ("ok"))
    , timeout = Nothing
    , withCredentials = False
    } 
    |> Http.send OnMixTurnOff
      

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





