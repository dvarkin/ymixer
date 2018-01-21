module Main exposing (..)

import Html exposing (program)
import Models exposing (Model, initialModel)
import Msgs exposing (Msg)
import Update exposing (update)
import View exposing (view)
import Commands exposing (fetchMixes)



init : ( Model, Cmd Msg )
init =
  ( initialModel, fetchMixes )


main : Program Never Model Msg
main =
  program
    { init = init
    , view = view 
    , update = update 
    , subscriptions = always Sub.none
    }