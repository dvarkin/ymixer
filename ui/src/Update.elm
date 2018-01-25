module Update exposing (..)

import Models exposing (Model, Route(..), Mdl, ChannelId, Channel)
import Msgs exposing (Msg(..))
import Router exposing (parseLocation)
import Commands exposing (fetchChannels)
import Material 
import Navigation
import RemoteData
import List.Extra 
import Debug


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

    ChangeCardSize howMuch ->
      let
        _ = Debug.log "howMuch" howMuch
        _ = Debug.log "newSize" model.cardSize + howMuch
      in
        model ! [ Cmd.none ]

    KeyMsg code ->
      case code of 
        187 -> -- '=' inc
          { model | cardSize = model.cardSize + 5 } ! [ Cmd.none ]

        189 -> -- '-' dec
          { model | cardSize = model.cardSize - 5 } ! [ Cmd.none ]

        _ ->
          model ! [ Cmd.none ]
          

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
    chans
      |> List.Extra.find (\ch -> ch.id == id)
      |> Maybe.map (\ch -> { ch | on = on })
      |> Maybe.map (replaceChan chans)
      |> Maybe.withDefault chans

