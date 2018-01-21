module Views.Mix exposing (..)

import Html exposing (Html, div, text, ul, li)
import Models exposing (Model, Channel, ChannelId)
import Msgs exposing (Msg)
import RemoteData exposing (WebData)
import List.Extra


view : Model -> Html Msg
view model =
  div []
    [ mixInfo model
    , maybeList model.channels
    ]


mixInfo : Model -> Html Msg 
mixInfo model =
  case model.mix of
    Just mixId ->
      case model.mixes of
        RemoteData.Success mixes ->
          case List.Extra.find (\mix -> mix.id == mixId) mixes of
            Just mix ->
              div [] [ text ("mix: id = " ++ (toString mix))]

            Nothing ->
              div [] [ text "can't find mix" ] 

        _ ->
          div [] [ text "can't get mixes" ]   
    
    Nothing ->
      div [] [ text "mix is not defined" ]    


maybeList : WebData (List Channel) -> Html Msg
maybeList response =
  case response of
    RemoteData.NotAsked ->
      div [] [ text "Not asked for channels" ]

    RemoteData.Loading ->
      div [] [ text "Loading channels..." ]

    RemoteData.Failure error ->
      div [] [ text (toString error) ]

    RemoteData.Success channels ->
      list channels


list : List Channel -> Html Msg
list channels =
  ul [] (List.map channelEntry channels)


channelEntry : Channel -> Html Msg 
channelEntry channel =
  li [] [ text ("channel: id = " 
    ++ (toString channel.id) 
    ++ " name = " 
    ++ channel.name
    ++ " on = "
    ++ (toString channel.on)) ]

