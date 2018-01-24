module Update exposing (..)

import Models exposing (Model, Route(..), Mdl, ChannelId, Channel)
import Msgs exposing (Msg(..))
import Router exposing (parseLocation)
import Commands exposing (fetchChannels)
import Material 
import Navigation
import RemoteData
import List.Extra 

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Mdl msg_ ->
      Material.update Mdl msg_ model

    NewUrl newUrl ->
      model ! [ Navigation.newUrl newUrl ]

    OnFetchMixes response ->
      { model | mixes = response } ! []

    OnFetchChannels response ->
      { model | channels = response } ! []

    SetChannel ( ch, on ) ->
      let 
        newChannes = RemoteData.map (updateChannel ch on) model.channels
      in 
        { model | channels = newChannes } ! []

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


updateChannel : ChannelId -> Bool -> List Channel -> List Channel
updateChannel id on chans =
  let 
    replaceChan chans chan =
      List.Extra.replaceIf (\ch -> ch.id == id) chan chans
  in
    List.Extra.find (\ch -> ch.id == id) chans
      |> Maybe.map (\ch -> { ch | on = on })
      |> Maybe.map (replaceChan chans)
      |> Maybe.withDefault chans
