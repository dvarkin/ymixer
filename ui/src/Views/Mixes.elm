module Views.Mixes exposing (..)

import Html exposing (Html, div, text, ul, li)
import Models exposing (Model, Mdl, Mix, MixId)
import Msgs exposing (Msg)
import RemoteData exposing (WebData)
import Material.Options as Options exposing (css)
import Material.Color as Color
import Material.Grid as Grid
import Material.Card as Card 
import Material.Button as Button
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
      div [] []

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
        , css "flex-wrap" "wrap"
        , css "align-items" "flex-start"
        , css "width" "100%"
        ]
        (List.map (mixCard model) mixes)
    ]



mixCard : Model -> Mix -> Html Msg 
mixCard {mdl, cardSize} {id} = 
  let
    title = 
      "Mix " ++ toString (id + 1)
  in    
    Card.view
      [ css "width" ((toString cardSize) ++ "px")
      , css "height" ((toString cardSize) ++ "px")
      , css "margin" "5px"
      , Color.background (Color.color Color.Teal Color.S500)
      , Options.onClick (Router.gotoMix id)
      ]
      [ Card.title [] 
        [ Card.head 
          [ Color.text Color.white ] 
          [ text title ] 
        ]
      , Card.text [ Card.expand ]  [] -- Filler
      , Card.actions
          [ Card.border
          , Options.center
          , Color.text Color.white
          ]
          [ Button.render Msgs.Mdl [0] mdl
              [ Button.colored
              , Button.raised
              , Color.background (Color.color Color.Teal Color.S400)
              , Options.onClick (Msgs.TurnMixOff id)
              ]
              [ text "Mute all"]
          ]
      ]
