module Update exposing (..)

import Models exposing (Model, Route(..), Mdl)
import Msgs exposing (Msg(..))
import Router exposing (parseLocation)
import Commands exposing (fetchChannels)
import Material 


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Mdl msg_ ->
      Material.update Mdl msg_ model

    OnFetchMixes response ->
      { model | mixes = response } ! []

    OnFetchChannels response ->
      { model | channels = response } ! []

    OnLocationChange location ->
      let 
        newRoute =
          parseLocation location

        cmds = 
          case newRoute of
            Models.MixRoute id ->
              [ fetchChannels id]

            _ ->
              []

        newMix =
          case newRoute of
            MixRoute id ->
              Just id

            _ ->
              Nothing
      in
        { model | route = newRoute, mix = newMix } ! cmds

