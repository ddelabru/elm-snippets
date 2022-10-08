module Blackout exposing (init)

import Array exposing (Array)
import Browser
import Css exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Html.Styled.Lazy exposing (..)


type alias Document msg =
  { title : String
  , body : List (Html.Html msg)
  }


main =
  Browser.element
    { init = init
    , view = view
    , subscriptions = subscriptions
    , update = update
    }


type Visibility
  = Blackout
  | Peeking
  | Visible


type alias Model =
  { attribution : String
  , textContent : String
  , visibilities : Array (Visibility)
  }


allBlackout : String -> Array (Visibility)
allBlackout s = Array.repeat (List.length <| String.words s) Blackout

quote : String
quote = "Nobody respects money anymore, neither those who have it nor those " ++
  "who don't.  When asked what they want to be some day, twenty percent of " ++
  "young Germans answer \"artist.\" Work is no longer endured as a given of " ++
  "the human condition. The  accounting departments of corporations confess " ++
  "that they have no idea where value comes from. The market's bad " ++
  "reputation would have done it in a decade ago if not for the bluster and " ++
  "fury, not to mention the deep pockets, of its apologists. It is common " ++
  "sense now to see progress as synonymous with disaster. In the world of " ++
  "the economic, everything is in flight, just like in the USSR under " ++
  "Andropov. Anyone who has spent a little time analyzing the final years " ++
  "of the USSR knows very well that the pleas for goodwill coming from our " ++
  "rulers, all of their fantasies about some future that has disappeared " ++
  "without a trace, all of their professions of faith in \"reforming\" this " ++
  "and that, are just the first fissures in the structure of the wall. The " ++
  "collapse of the socialist bloc was in no way victory of capitalism; it " ++
  "was merely the bankrupting of one of the forms capitalism takes. " ++
  "Besides, the demise of the USSR did not come about because a people " ++
  "revolted, but because the nomenclature was undergoing a process of " ++
  "reconversion. When it proclaimed the end of socialism, a small fraction " ++
  "of the ruling class emancipated itself from the anachronistic duties " ++
  "that still bound it to the people. It took private control of what it " ++
  "already controlled in the name of \"everyone.\" In the factories, the " ++
  "joke went: \"we pretend to work, they pretend to pay us.\" The oligarchy " ++
  "replied, \"there's no point, let's stop pretending!\" They ended up with " ++
  "the raw materials, industrial infrastructures, the military-industrial " ++
  "complex, the banks and the nightclubs. Everyone else got poverty or "++
  "emigration. Just as no one in Andropov's time believed in the USSR, no " ++
  "one in the meeting halls, workshops and offices believes in France " ++
  "today. \"There's no point,\" respond the bosses and political leaders, " ++
  "who no longer even bother to file the edges off the \"iron laws of the " ++
  "economy.\" They strip factories in the middle of the night and announce " ++
  "the shutdown early next morning. They no longer hesitate to send in " ++
  "anti-terrorism units to shut down a strike, like with the ferries and " ++
  "the occupied recycling center in Rennes. The brutal activity of power " ++
  "today consists both in administering this ruin while, at the same time, " ++
  "establishing the framework for a \"new economy.\""

init : () -> (Model, Cmd Msg)
init _ =
  ( Model "—The Invisible Committee" quote (allBlackout quote)
  , Cmd.none
  )


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


type Msg
  = ChangeAttribution String
  | ChangeText String
  | Hide Int
  | NoChange
  | Peek Int
  | Reveal Int


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ChangeAttribution newAttribution ->
      ( { model | attribution = newAttribution }
      , Cmd.none
      )
    ChangeText newText ->
      ( { model | textContent = newText, visibilities = (allBlackout newText) }
      , Cmd.none
      )
    Hide i ->
      ( { model | visibilities = (Array.set i Blackout model.visibilities) }
      , Cmd.none
      )
    NoChange -> (model, Cmd.none)
    Peek i ->
      ( { model | visibilities = (Array.set i Peeking model.visibilities) }
      , Cmd.none
      )
    Reveal i ->
      ( { model | visibilities = (Array.set i Visible model.visibilities) }
      , Cmd.none
      )


wordElements : Model -> List (Html.Styled.Html Msg)
wordElements model =
  List.indexedMap ( \i w ->
    span [ css
      [ backgroundColor (
        if (Array.get i model.visibilities) == Just Blackout then hex "000"
        else hex "fff"
        )
      , color (hex "000")
      , display inlineBlock
      , Css.height (Css.em 1.5)
      , paddingLeft (px 3)
      , paddingRight (px 3)
      ]
    , onClick (
      if (Array.get i model.visibilities) == Just Visible then Hide i
      else Reveal i
    )
    , onMouseOver (
      if (Array.get i model.visibilities) == Just Blackout then Peek i
      else NoChange
    )
    , onMouseOut (
      if (Array.get i model.visibilities) == Just Peeking then Hide i
      else NoChange
    )
    ] [ text (w ++ " ") ]
  ) (String.words model.textContent)


view : Model -> Html.Html Msg
view model = toUnstyled <| lazy2 div
  [ css
    [ textAlign right
    ]
  ]
  [ textarea
    [ css
      [ Css.width (pct 100)
      , overflow auto
      , resize none
      ]
    , onInput ChangeText
    , rows 10
    , value model.textContent
    ]
    []
  , br [] []
  , input
    [ onInput ChangeAttribution
    , value model.attribution] []
  , br [] []
  , div
    [ css
      [ backgroundColor (hex "fff")
      , color (hex "000")
      , marginTop (px 20)
      , padding (px 10)
      ]
    ]
    [ div [css [lineHeight (num 1.5), textAlign left]] (wordElements model)
    , div [css [textAlign right]] [text model.attribution]
    ]
  ]
