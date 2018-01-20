module View exposing (..)

import Html exposing (Html, div, text)
import Models exposing (..)
import Msgs exposing (..)

view : Model -> Html Msg
view model =
  div [] [ text model ]