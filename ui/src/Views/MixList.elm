module Views.MixList exposing (..)

import Html exposing (Html, div, text, ul, li, a)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import Models exposing (Model, Mix, MixId)
import Msgs exposing (Msg)
import RemoteData exposing (WebData)
import Router exposing (mixPath)
import Material.List as Lists
import Material.Options as Options exposing (css)
import Material.Color as Color
import Material.Typography as Typography
import Material.Grid as Grid


view : Model -> Html Msg
view model =
  Grid.grid [] 
  [ Grid.cell 
    [ Grid.size Grid.Tablet 8, Grid.size Grid.Desktop 5 ]
    [ Options.styled Html.h4 [ Typography.headline ] [ text "Mixes" ]
    , maybeList model.mixes
    ]
  ]


maybeList : WebData (List Mix) -> Html Msg 
maybeList response =
  case response of
    RemoteData.NotAsked ->
      text "Not asked for mix list"

    RemoteData.Loading ->
      text "Loading..."

    RemoteData.Failure error ->
      text (toString error)

    RemoteData.Success mixes ->
      list mixes


list : List Mix -> Html Msg
list mixes =
  Lists.ul [ css "margin" "0", css "padding" "0" ] (List.map mixEntry mixes)


mixEntry : Mix -> Html Msg 
mixEntry mix =
  Lists.li [] 
    [ Options.div 
      [ Options.center
      , Color.text Color.accentContrast
      , Typography.title
      , css "width" "36px"
      , css "height" "36px"
      , css "margin-right" "2rem"
      ] 
      [ text (toString mix.id) ]
    , Lists.content 
      [ Options.attribute <| onClick (Msgs.NewUrl (mixPath mix.id)) ] 
      [ text mix.name ]
    ]
    


