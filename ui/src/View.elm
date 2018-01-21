module View exposing (..)

import Html exposing (Html, div, text, h5)
import Html.Attributes exposing (style)
import Models exposing (..)
import Msgs exposing (..)
import Views.MixList as MixList
import Views.Mix as Mix
import Material
import Material.Scheme
import Material.Layout as Layout
import Material.Color as Color

view : Model -> Html Msg 
view model =
  Material.Scheme.topWithScheme Color.Teal Color.LightGreen <|
    Layout.render Mdl 
      model.mdl 
      [ Layout.fixedHeader
      ]
      { header = [ h5 [ style [ ( "padding", "2rem" ) ]] [ text "YMixer" ]]
      , drawer = []
      , tabs = ( [], [] )
      , main = [ viewBody model ]
      }


viewBody : Model -> Html Msg 
viewBody model =
  case model.route of
    Models.MixesRoute ->
      MixList.view model 

    Models.MixRoute id ->
      Mix.view model
      --div [] [ text (toString id) ]

    Models.NotFoundRoute ->
      div [] [ text "not found" ]



