module View exposing (..)

import Html exposing (Html, div, text, h5)
import Models exposing (..)
import Msgs exposing (..)
import Views.Mixes as Mixes
import Views.Channels as Channels
import Views.ManageChannels as ManChans
import Material.Scheme
import Material.Layout as Layout
import Material.Color as Color
import Material.Options as Options
import Router

view : Model -> Html Msg 
view model = 
  Material.Scheme.topWithScheme Color.Teal Color.LightGreen <|
    Layout.render Mdl 
      model.mdl 
      [ Layout.fixedHeader
      , Layout.selectedTab model.selectedTab
      , Layout.onSelectTab Msgs.SelectTab
      ]
      { header = [ Layout.row [] 
                    [ Layout.title 
                      [ Options.onClick (Router.gotoMixes)] 
                      [ text "Mixer" ]] ]
      , drawer = []
      , tabs = ( [ text "Control", text "Manage" ]
               , [ Color.background (Color.color Color.Teal Color.S400) ] 
               )
      , main = [ viewBody model ]
      }


viewBody : Model -> Html Msg 
viewBody ({ route, selectedTab } as model) =
  case ( route, selectedTab ) of
    ( _, 1 ) ->
      ManChans.view model

    ( Models.MixesRoute, 0 ) ->
      Mixes.view model 

    ( Models.MixRoute id, 0 ) ->
      Channels.view model

    ( Models.NotFoundRoute, _ ) ->
      div [] [ text "not found" ]

    ( _, _ ) ->
      div [] [ text "not found combination" ]



