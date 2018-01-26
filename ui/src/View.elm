module View exposing (..)

import Html exposing (Html, div, text, h5)
import Models exposing (..)
import Msgs exposing (..)
import Views.Mixes as Mixes
import Views.Channels as Channels
import Views.ManageChannels as ManChans
import Material.Scheme
import Material.Layout as Layout
import Material.Color as Color
import Material.Options as Options
import Router

view : Model -> Html Msg 
view model = 
  let
    (title, navigation) =
      case model.mix of
        Just id ->
          ( "Mix " ++ toString (id + 1), [ editPhotosNavidation ] )

        Nothing ->
          ( "Mixer", [] )
  in   
    Material.Scheme.topWithScheme Color.Teal Color.LightGreen <|
      Layout.render Mdl 
        model.mdl 
        [ Layout.fixedHeader
        ]
        { header = [ Layout.row [] 
                      [ Layout.title 
                          [ Options.onClick (Router.gotoMixes)] 
                          [ text title ]
                      , Layout.spacer
                      , Layout.navigation [] navigation
                      ]
                    ]
        , drawer = []
        , tabs = ( [], [] )
        , main = [ viewBody model ]
        }


viewBody : Model -> Html Msg 
viewBody ({ route, editPhotos } as model) =
  case ( route, editPhotos ) of
    ( Models.MixesRoute, _ ) ->
      Mixes.view model 

    ( Models.MixRoute id, False ) ->
      Channels.view model

    ( Models.MixRoute id, True ) ->
      ManChans.view model 

    ( Models.NotFoundRoute, _ ) ->
      div [] [ text "not found" ]


editPhotosNavidation : Html Msg
editPhotosNavidation = 
  Layout.link
    [ Options.onClick Msgs.EditPhotos] 
    [ text "Edit photos" ]
