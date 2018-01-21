module Main exposing (..)

import Models exposing (Model, initialModel)
import Msgs exposing (Msg)
import Update exposing (update)
import View exposing (view)
import Commands exposing (fetchMixes)
import Navigation exposing (Location)
import Router


init : Location -> ( Model, Cmd Msg )
init location =
  let 
    currentRoute =
      Router.parseLocation location
  in
    ( initialModel currentRoute, fetchMixes )


main : Program Never Model Msg
main =
  Navigation.program Msgs.OnLocationChange
    { init = init
    , view = view 
    , update = update 
    , subscriptions = always Sub.none
    }