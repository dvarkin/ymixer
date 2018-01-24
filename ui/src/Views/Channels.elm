module Views.Channels exposing (..)

import Html exposing (Html, div, text, ul, li)
import Models exposing (Model, Mdl, Channel, ChannelId)
import Msgs exposing (Msg)
import RemoteData exposing (WebData)
import Material.Options as Options exposing (css)
import Material.Color as Color
import Material.Typography as Typography
import Material.Grid as Grid
import Material.Card as Card 

view : Model -> Html Msg
view model =
  Grid.grid [] 
  [ Grid.cell 
    [ Grid.size Grid.Desktop 12
    , Grid.size Grid.Tablet 8
    , Grid.size Grid.Phone 4 
    ]
    [ maybeDashboard model ]
  ]


maybeDashboard : Model -> Html Msg
maybeDashboard model =
  case model.channels of
    RemoteData.NotAsked ->
      div [] [ text "Not asked for channels" ]

    RemoteData.Loading ->
      div [] [ text "Loading channels..." ]

    RemoteData.Failure error ->
      div [] [ text (toString error) ]

    RemoteData.Success channels ->
      dashboard model.mdl channels


dashboard : Mdl -> (List Channel) -> Html Msg 
dashboard mdl chans =
  Options.div 
    [ css "display" "flex"
    --, css "flex-flow" "row wrap"
    , css "flex-wrap" "wrap"
    --, css "justify-content" "space-between"
    , css "align-items" "flex-start"
    , css "width" "100%"
    ]
    (List.map (chanCard mdl) chans)


chanCard : Mdl -> Channel -> Html Msg 
chanCard mdl {id, image, on} = 
  let
    (title, color) =
      case on of
        True ->
          ( "On", Color.color Color.Green Color.A400 )
        False ->
          ( "Off", Color.color Color.Grey Color.S600 )
  in    
    Card.view
      [ css "width" "150px"
      , css "height" "150px"
      , css "margin" "5px"
      , css "background" ("url('" ++ image ++ "') center / cover")
      , Options.onClick (Msgs.SetChannel (id, not on))
      ]
      [ Card.text [ Card.expand ] [] -- Filler
      , Card.text
          [ css "background" "rgba(0, 0, 0, 0.5)" ] -- Non-gradient scrim
          [ Options.span
              [ Color.text color
              --, Typography.left
              , Typography.contrast 1.0 ]
              [ text title ]
          ]
      ]

