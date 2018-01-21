module Update exposing (..)

import Models exposing (Model, Route(..), Mdl, ChannelId)
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
      (updateChannel model ch on) ! []

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


updateChannel : Model -> ChannelId -> Bool -> Model
updateChannel model id on =
  case model.channels of
    RemoteData.Success channels ->
      case List.Extra.find (\ch -> ch.id == id) channels of 
        Just chan ->
          let
            updatedChan = 
              { chan | on = on }

            pick currentChan =
              if currentChan.id == updatedChan.id then
                updatedChan
              else
                currentChan

            updateChanList channels =
              List.map pick channels

            newChannels =
              RemoteData.map updateChanList model.channels

          in
            { model | channels = newChannels }
        _ ->
          model
    _ ->
      model 


--updatePlayer : Model -> Player -> Model
--updatePlayer model updatedPlayer =
--  let
--    pick currentPlayer =
--      if updatedPlayer.id == currentPlayer.id then
--        updatedPlayer
--      else
--        currentPlayer
      
--    updatePlayerList players =
--      List.map pick players 

--    updatedPlayers =
--      RemoteData.map updatePlayerList model.players
--  in
--    { model | players = updatedPlayers }

--mixInfo model =
--  case model.mix of
--    Just mixId ->
--      case model.mixes of
--        RemoteData.Success mixes ->
--          case List.Extra.find (\mix -> mix.id == mixId) mixes of
--            Just mix ->
--              Options.styled Html.h4 
--                [ Typography.headline ] 
--                [ text mix.name ]

--            Nothing ->
--              div [] [ text "can't find mix" ] 

--        _ ->
--          div [] [ text "can't get mixes" ]   
    
--    Nothing ->
--      div [] [ text "mix is not defined" ]    

