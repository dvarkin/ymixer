module View exposing (..)

import Html exposing (Html)
import Models exposing (..)
import Msgs exposing (..)
import Views.MixList as MixList


view : Model -> Html Msg
view model =
  MixList.view model