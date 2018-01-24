module Views.Mix exposing (..)

import Html exposing (Html, div, text, ul, li)
import Models exposing (Model, Mdl, Channel, ChannelId)
import Msgs exposing (Msg)
import RemoteData exposing (WebData)
import List.Extra
import Material.List as Lists
import Material.Options as Options exposing (css)
import Material.Color as Color
import Material.Typography as Typography
import Material.Grid as Grid
import Material.Toggles as Toggles


view : Model -> Html Msg
view model =
  Grid.grid [] 
  [ Grid.cell 
    [ Grid.size Grid.Desktop 12
    , Grid.size Grid.Tablet 8
    , Grid.size Grid.Phone 4 
    ]
    [ mixInfo model
    , maybeList model
    ]
  ]

mixInfo : Model -> Html Msg 
mixInfo model =
  case model.mix of
    Just mixId ->
      case model.mixes of
        RemoteData.Success mixes ->
          case List.Extra.find (\mix -> mix.id == mixId) mixes of
            Just mix ->
              Options.styled Html.h4 
                [ Typography.headline ] 
                [ text mix.name ]

            Nothing ->
              div [] [ text "can't find mix" ] 

        _ ->
          div [] [ text "can't get mixes" ]   
    
    Nothing ->
      div [] [ text "mix is not defined" ]    


maybeList : Model -> Html Msg
maybeList model =
  case model.channels of
    RemoteData.NotAsked ->
      div [] [ text "Not asked for channels" ]

    RemoteData.Loading ->
      div [] [ text "Loading channels..." ]

    RemoteData.Failure error ->
      div [] [ text (toString error) ]

    RemoteData.Success channels ->
      list model.mdl channels


list : Mdl -> List Channel -> Html Msg
list mdl channels  =
  Lists.ul [] (List.map (channelEntry mdl) channels)


channelEntry : Mdl -> Channel -> Html Msg 
channelEntry mdl ch =
  Lists.li [ Lists.withSubtitle ] 
    [ Lists.content 
      []
      [ Lists.avatarImage ch.image []
      , text ch.name
      , Lists.subtitle [] [ text ("Channel #" ++ toString ch.id) ]
      ]
    , Lists.content2 []
        [ Toggles.switch Msgs.Mdl [0] mdl
          [ Toggles.value ch.on
          , Options.onToggle (Msgs.SetChannel (ch.id, not ch.on))
          ]
          []
        ]
    ]
