module Commands exposing (..)

import Http
import Json.Decode as Decode 
import Json.Decode.Pipeline exposing (decode, required)
import RemoteData
import Models exposing (Mix)
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


