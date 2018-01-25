module Views.Mixes exposing (..)

import Html exposing (Html, div, text, ul, li)
import Models exposing (Model, Mdl, Mix, MixId)
import Msgs exposing (Msg)
import RemoteData exposing (WebData)
import Material.Options as Options exposing (css)
import Material.Color as Color
import Material.Typography as Typography
import Material.Grid as Grid
import Material.Card as Card 
import Router

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
  case model.mixes of
    RemoteData.NotAsked ->
      div [] [ text "Not asked for mixes" ]

    RemoteData.Loading ->
      div [] [ text "Loading mixes..." ]

    RemoteData.Failure error ->
      div [] [ text (toString error) ]

    RemoteData.Success mixes ->
      dashboard model mixes


dashboard : Model -> (List Mix) -> Html Msg 
dashboard model mixes =
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
        (List.map (mixCard model) mixes)
    ]



mixCard : Model -> Mix -> Html Msg 
mixCard {mdl, cardSize} {id} = 
  let
    title = 
      "Mix " ++ toString id
  in    
    Card.view
      [ css "width" ((toString cardSize) ++ "px")
      , css "height" ((toString cardSize) ++ "px")
      , css "margin" "5px"
      , Color.background (Color.color Color.Teal Color.S500)
      , Options.onClick (Msgs.NewUrl (Router.mixPath id))
      ]
      [ Card.title [] [ Card.head [ Color.text Color.white ] [ text title ] ]
      --, Card.text
      --    [ css "background" "rgba(0, 0, 0, 0.5)" ] -- Non-gradient scrim
      --    [ Options.span
      --        [ Color.text Color.white
      --        , Typography.contrast 1.0 ]
      --        [ text title ]
      --    ]
      ]

