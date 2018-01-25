module View exposing (..)

import Html exposing (Html, div, text, h5)
--import Html.Attributes exposing (style)
import Models exposing (..)
import Msgs exposing (..)
import Views.MixList as MixList
--import Views.Mix as Mix
import Views.Mixes as Mixes
import Views.Channels as Channels
import Material.Scheme
import Material.Layout as Layout
import Material.Color as Color

view : Model -> Html Msg 
view model =
  let
    tabs =
      case model.mix of
        Just _ ->
          ( [ text "Control", text "Manage" ]
          , [ Color.background (Color.color Color.Teal Color.S400) ] 
          )

        Nothing ->
          ( [], [] )
  in    
    Material.Scheme.topWithScheme Color.Teal Color.LightGreen <|
      Layout.render Mdl 
        model.mdl 
        [ Layout.fixedHeader
        , Layout.selectedTab 0
        ]
        { header = [ Layout.row [] 
                      [ Layout.title [] [ text "Mixer" ]] ]
        , drawer = []
        , tabs = tabs
        , main = [ viewBody model ]
        }


viewBody : Model -> Html Msg 
viewBody model =
  case model.route of
    Models.MixesRoute ->
      Mixes.view model 

    Models.MixRoute id ->
      Channels.view model
      --div [] [ text (toString id) ]

    Models.NotFoundRoute ->
      div [] [ text "not found" ]



