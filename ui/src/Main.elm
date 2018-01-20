module Main exposing (..)

import Html exposing (program)
import Models exposing (Model, initialModel)
import View exposing (view)
import Update exposing (update)
import Msgs exposing (..)


init : ( Model, Cmd Msg )
init =
  ( initialModel, Cmd.none )

main : Program Never Model Msg
main =
  program
    { init = init
    , view = view 
    , update = update 
    , subscriptions = always Sub.none
    }