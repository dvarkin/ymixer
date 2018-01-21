module View exposing (..)

import Html exposing (Html, div, text)
import Models exposing (..)
import Msgs exposing (..)
import Views.MixList as MixList


view : Model -> Html Msg
view model =
  page model


page : Model -> Html Msg 
page model =
  case model.route of
    Models.MixesRoute ->
      MixList.view model 

    Models.MixRoute id ->
      div [] [ text (toString id) ]

    Models.NotFoundRoute ->
      div [] [ text "not found" ]



