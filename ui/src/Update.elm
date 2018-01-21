module Update exposing (..)

import Models exposing (Model)
import Msgs exposing (Msg(..))
import Router exposing (parseLocation)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    OnFetchMixes response ->
      { model | mixes = response } ! []

    OnLocationChange location ->
      let 
        newRoute =
          parseLocation location
      in
        { model | route = newRoute } ! []

