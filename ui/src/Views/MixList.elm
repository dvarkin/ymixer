module Views.MixList exposing (..)

import Html exposing (Html, div, text, ul, li)
import Models exposing (Model, Mix)
import Msgs exposing (Msg)
import RemoteData exposing (WebData)


view : Model -> Html Msg
view model =
  div [] [ maybeList model.mixes ]


maybeList : WebData (List Mix) -> Html Msg 
maybeList response =
  case response of
    RemoteData.NotAsked ->
      text "Not asked for mix list"

    RemoteData.Loading ->
      text "Loading..."

    RemoteData.Failure error ->
      text (toString error)

    RemoteData.Success mixes ->
      list mixes


list : List Mix -> Html Msg
list mixes =
  ul []
    (List.map mixEntry mixes)


mixEntry : Mix -> Html Msg 
mixEntry mix =
  li []
    [ text ((toString mix.id) ++ ": " ++ mix.name) ]


