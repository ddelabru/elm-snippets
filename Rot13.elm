module Rot13 exposing (init, rot13)

import Browser
import Css exposing (..)
import Html exposing (Html)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Html.Styled.Lazy exposing (..)

rot13 : String -> String
rot13 s =
  String.map (
    \c ->
      if Char.isUpper c then
        Char.fromCode (
          (modBy 26 ((Char.toCode c) - (Char.toCode 'A') + 13)) +
          (Char.toCode 'A')
        )
      else if Char.isLower c then
        Char.fromCode (
          (modBy 26 ((Char.toCode c) - (Char.toCode 'a') + 13)) +
          (Char.toCode 'a')
        )
      else c
  ) s

main =
  Browser.element
    { init = init
    , view = view
    , subscriptions = subscriptions
    , update = update
    }


type alias Model = String


init : () -> (Model, Cmd Msg)
init _ = ("Bhgchg grkg nccrnef urer" , Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


type Msg
  = ChangeText String


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ChangeText newText -> (newText, Cmd.none)


view : Model -> Html.Html Msg
view model =
  toUnstyled <| lazy2 div
  []
  [ textarea
    [ css
      [ Css.width (pct 100)
      , overflow auto
      , resize none
      ]
    , onInput ChangeText
    , rows 10
    , value model
    ] []
  , Html.Styled.pre
    [ css
      [ backgroundColor (hex "fff")
      , color (hex "000")
      , Css.width (pct 100)
      , overflowWrap normal
      , textAlign left
      , whiteSpace preWrap
      ]
    ] [text (rot13 model)]
  ]
