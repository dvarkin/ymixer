module Update exposing (..)

import Models exposing (Model, Mix, MixId, Route(..), Mdl, ChannelId, Channel)
import Msgs exposing (Msg(..))
import Router exposing (parseLocation)
import Commands exposing (fetchChannels, turnMixOff, setChannel)
import Material 
import Navigation
import RemoteData exposing (WebData)
import List.Extra 
import Debug
import Http


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Mdl msg_ ->
      Material.update Mdl msg_ model

    EditPhotos ->
      { model | editPhotos = not model.editPhotos } ! []

    NewUrl newUrl ->
      model ! [ Navigation.newUrl newUrl ]

    OnMixTurnOff _ ->
      model ! []

    TurnMixOff id ->
      model ! [ turnMixOff id ] 

    SetChannel ( mix, ch, on ) ->
      model ! [ setChannel mix ch on ]

    OnSetChannel (Ok (mix, chan, on)) ->
      let
        newChans = 
          tryUpdateChannel mix chan on model
      in          
        { model | channels = newChans } ! []       

    OnSetChannel (Err _) ->
      let
        _ = 
          Debug.log "failed to set channel"
      in          
        model ! []

    OnFetchMixes response ->
      let
        mixes =
          RemoteData.map (\ids -> List.map (\id -> Mix id) ids) response
      in
        { model | mixes = mixes } ! []

    OnFetchChannels response ->
      { model | channels = response } ! []

    KeyMsg code ->
      case code of 
        187 -> -- '=' inc
          { model | cardSize = model.cardSize + 5 } ! []

        189 -> -- '-' dec
          { model | cardSize = model.cardSize - 5 } ! []

        _ ->
          model ! []
          
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

        newChannels =
          case newRoute of 
            MixesRoute ->
              RemoteData.NotAsked

            MixRoute _ ->
              RemoteData.Loading

            _ ->
              model.channels
      in
        { model 
          | route = newRoute
          , mix = newMix
          , channels = newChannels
        } ! cmds


tryUpdateChannel : MixId -> ChannelId -> Bool -> Model -> WebData (List Channel)
tryUpdateChannel mix chan on model =
  case model.mix of
    Just mixId ->
      if mixId == mix then
        RemoteData.map (updateChannel chan on) model.channels
      else
        model.channels
    Nothing ->
      model.channels


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

