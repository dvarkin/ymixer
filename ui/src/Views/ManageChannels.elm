module Views.ManageChannels exposing (..)

import Html exposing (Html, div, text, ul, li, p)
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
    [ Options.styled p
      [ Typography.subhead ]
      [ text "Hint: you can always use +/- keys to change button size" ]
    , maybeDashboard model 
    ]
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
      dashboard model channels


dashboard : Model -> (List Channel) -> Html Msg 
dashboard model chans =
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
        (List.map (chanCard model) chans)
    ]



chanCard : Model -> Channel -> Html Msg 
chanCard {mdl, cardSize} {id, image, on} = 
  let
    title =
      "Upload"
  in    
    Card.view
      [ css "width" ((toString cardSize) ++ "px")
      , css "height" ((toString cardSize) ++ "px")
      , css "margin" "5px"
      , css "background" ("url('" ++ image ++ "') center / cover")
      --, Options.onClick (Msgs.SetChannel (id, not on))
      ]
      [ Card.text [ Card.expand ] [] -- Filler
      , Card.text
          [ css "background" "rgba(0, 0, 0, 0.5)" ]
          [ Options.span
              [ Color.text Color.white
              --, Typography.left
              , Typography.contrast 1.0 ]
              [ text "Upload" ]
          ]
      ]

