module Main exposing (..)

import Models exposing (Model, Route(..), initialModel)
import Msgs exposing (Msg)
import Update exposing (update)
import View exposing (view)
import Commands exposing (fetchMixes, fetchChannels)
import Navigation exposing (Location)
import Router
import Keyboard

init : Location -> ( Model, Cmd Msg )
init location =
  let 
    currentRoute =
      Router.parseLocation location      
  in
    case currentRoute of
      MixesRoute ->
        (initialModel currentRoute Nothing) ! [ fetchMixes ]

      MixRoute id ->
        (initialModel currentRoute (Just id)) ! [ fetchMixes, fetchChannels id]

      _ ->
        (initialModel currentRoute Nothing) ! []


subscriptions : Model -> Sub Msg
subscriptions model =
  Keyboard.downs Msgs.KeyMsg


main : Program Never Model Msg
main =
  Navigation.program Msgs.OnLocationChange
    { init = init
    , view = view 
    , update = update 
    , subscriptions = subscriptions
    }