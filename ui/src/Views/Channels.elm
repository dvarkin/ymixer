module Views.Channels exposing (..)

import Html exposing (Html, div, text, ul, li)
import Models exposing (Model, Mdl, Channel, ChannelId, MixId)
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
maybeDashboard ({channels, mix} as model) =
  case (channels, mix) of
    ( RemoteData.NotAsked, _ ) ->
      div [] [ text "Not asked for channels" ]

    ( RemoteData.Loading, _ ) ->
      div [] []
      --Loading.indeterminate

    ( RemoteData.Failure error, _ ) ->
      div [] [ text (toString error) ]

    ( RemoteData.Success channels, Nothing ) ->
      div [] []

    ( RemoteData.Success channels, Just mixId ) ->
      dashboard model channels mixId


dashboard : Model -> (List Channel) -> MixId-> Html Msg 
dashboard model chans mix =
  Options.div
    [ Options.center
    , css "width" "100%"
    ]
    [ Options.div 
        [ css "display" "flex"
        --, css "flex-flow" "row wrap"
        , css "flex-wrap" "wrap"
        --, css "justify-content" "space-between"
        , css "align-items" "flex-start"
        , css "width" "100%"
        ]
        (List.map (chanCard model mix) chans)
    ]



chanCard : Model -> MixId-> Channel -> Html Msg 
chanCard {mdl, cardSize} mix {id, image, on} = 
  let
    (title, color) =
      case on of
        True ->
          ( "On", Color.color Color.Teal Color.A400 )
        False ->
          ( "Off", Color.color Color.Grey Color.S600 )
    name =
      "Ch " ++ toString (id + 1)
  in    
    Card.view
      [ css "width" ((toString cardSize) ++ "px")
      , css "height" ((toString cardSize) ++ "px")
      , css "margin" "5px"
      , css "background" ("url('" ++ image ++ "') center / cover")
      , Options.onClick (Msgs.SetChannel (mix, id, not on))
      ]
      [  Card.text
          [ Card.expand, Color.text (Color.color Color.Grey Color.S500) ] 
          [ text name ] 
          --Card.text [ Card.expand ] [] -- Filler
      , Card.text
          [ css "background" "rgba(0, 0, 0, 0.2)" ] -- Non-gradient scrim
          [ Options.span
              [ Color.text color
              --, Typography.left
              , Typography.contrast 1.0 ]
              [ text title ]
          ]
      ]

