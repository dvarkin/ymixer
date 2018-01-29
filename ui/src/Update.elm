module Update exposing (..)

import Models exposing (Model, Mix, MixId, Route(..), Mdl, ChannelId, Channel)
import Msgs exposing (Msg(..))
import Router exposing (parseLocation)
import Commands exposing (fetchChannels, turnMixOff, setChannel)
import Material 
import Navigation exposing (Location)
import RemoteData exposing (WebData)
import List.Extra 
import Ports exposing (uploadImage)
import Debug


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Mdl msg_ ->
      Material.update Mdl msg_ model

    EditPhotos ->
      { model | editPhotos = not model.editPhotos } ! []

    NavigateToUrl newUrl ->
      model ! [ Navigation.newUrl newUrl ]

    OnMuteMix _ ->
      model ! []

    MuteMix id ->
      model ! [ turnMixOff id ] 

    SetChannel ( mix, ch, on ) ->
      model ! [ setChannel mix ch on ]

    OnSetChannel (Ok (mix, chan, on)) ->
      tryUpdateChannel mix chan on model

    OnSetChannel (Err _) ->
      Debug.log "failed to set channel" model ! []

    OnFetchMixes response ->
        { model
          | mixes
              = RemoteData.map (\ids ->
                  List.map (\id -> Mix id) ids)
                  response
        } ! []

    OnFetchChannels response ->
      { model | channels = response } ! []

    KeyMsg code ->
      updateCardSize code model
          
    OnLocationChange location ->
      handleLocationChange location model

    UploadImage ch ->
      model ! [ uploadImage ch ]


tryUpdateChannel : MixId -> ChannelId -> Bool -> Model -> ( Model, Cmd Msg )
tryUpdateChannel mix chan on model =
  let
    newChans =
      case model.mix of
        Just mixId ->
          if mixId == mix then
            RemoteData.map (updateChannel chan on) model.channels
          else
            model.channels
        Nothing ->
          model.channels
  in
    { model | channels = newChans } ! []


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


updateCardSize : Int -> Model -> ( Model, Cmd Msg )
updateCardSize code model =
  let
    howMuch =
      case code of
        187 -> -5 -- dec (-)
        189 ->  5 -- inc (=)
        _   ->  0 -- ignore
  in
    { model | cardSize = model.cardSize - howMuch } ! []


handleLocationChange : Location -> Model -> ( Model, Cmd Msg )
handleLocationChange location model =
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