module GregorianToIfc exposing (init)

import Browser
import Date exposing (Date, ordinalDay) -- justinmimbs/date
import DatePicker exposing (DateEvent(..), defaultSettings)  -- CurrySoftware/elm-datepicker
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (..)
import Task
import Time exposing (Month(..), Weekday(..), now)

main =
  Browser.element
    { init = init
    , view = view
    , subscriptions = subscriptions
    , update = update
    }

isLeapYear : Int -> Bool
isLeapYear year =
  Date.ordinalDay (Date.fromCalendarDate year Dec 31) == 366

type alias Model =
  { date: Maybe Date
  , datePicker: DatePicker.DatePicker
  }

init : () -> (Model, Cmd Msg)
init _ =
  let
    ( datePicker, datePickerCmd ) = DatePicker.init
  in ( 
    { date = Nothing
    , datePicker = datePicker }
    , Cmd.map SetDatePicker datePickerCmd
    )

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

type Msg
  = SetDatePicker DatePicker.Msg

someSettings : DatePicker.Settings
someSettings =
  { defaultSettings
    | inputClassList = [ ( "form-control", True ) ]
    , inputId = Just "datepicker"
  }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SetDatePicker subMsg ->
      let
        ( newDatePicker, dateEvent ) =
          DatePicker.update someSettings subMsg model.datePicker

        date =
          case dateEvent of
            Picked newDate ->
              Just newDate

            _ ->
              model.date

      in
        ({ model
        | date = date
        , datePicker = newDatePicker
        }
        , Cmd.none)

type IfcMonth
  = JanIfc
  | FebIfc
  | MarIfc
  | AprIfc
  | MayIfc
  | JunIfc
  | SolIfc
  | JulIfc
  | AugIfc
  | SepIfc
  | OctIfc
  | NovIfc
  | DecIfc

type alias IfcDate =
  { year: Int
  , month: IfcMonth
  , day: Int
  }

ifcDateFromCalendarDate : Int -> IfcMonth -> Int -> IfcDate
ifcDateFromCalendarDate year month day =
  { year = year
  , month = month
  , day = day
  }

gregorianMonthToString : Time.Month -> String
gregorianMonthToString month = case month of
  Jan -> "January"
  Feb -> "February"
  Mar -> "March"
  Apr -> "April"
  May -> "May"
  Jun -> "June"
  Jul -> "July"
  Aug -> "August"
  Sep -> "September"
  Oct -> "October"
  Nov -> "November"
  Dec -> "December"

ifcMonthToString : IfcMonth -> String
ifcMonthToString month = case month of
  JanIfc -> "January"
  FebIfc -> "February"
  MarIfc -> "March"
  AprIfc -> "April"
  MayIfc -> "May"
  JunIfc -> "June"
  SolIfc -> "Sol"
  JulIfc -> "July"
  AugIfc -> "August"
  SepIfc -> "September"
  OctIfc -> "October"
  NovIfc -> "November"
  DecIfc -> "December"

weekdayToString : Time.Weekday -> String
weekdayToString weekday = case weekday of
  Sun -> "Sunday"
  Mon -> "Monday"
  Tue -> "Tuesday"
  Wed -> "Wednesday"
  Thu -> "Thursday"
  Fri -> "Friday"
  Sat -> "Saturday"

gregorianDateToIfcDate : Date -> IfcDate
gregorianDateToIfcDate date =
  if ordinalDay date < 29 then
    ifcDateFromCalendarDate (Date.year date) JanIfc (ordinalDay date)
  else if ordinalDay date < 57 then
    ifcDateFromCalendarDate (Date.year date) FebIfc ((ordinalDay date) - 28)
  else if ordinalDay date < 85 then
    ifcDateFromCalendarDate (Date.year date) MarIfc ((ordinalDay date) - 56)
  else if ordinalDay date < 113 then
    ifcDateFromCalendarDate (Date.year date) AprIfc ((ordinalDay date) - 84)
  else if ordinalDay date < 141 then
    ifcDateFromCalendarDate (Date.year date) MayIfc ((ordinalDay date) - 112)
  else if ordinalDay date < 169 then
    ifcDateFromCalendarDate (Date.year date) JunIfc ((ordinalDay date) - 140)
  else if isLeapYear (Date.year date) then
    if ordinalDay date == 169 then
      ifcDateFromCalendarDate (Date.year date) JunIfc 29
    else if ordinalDay date < 198 then
      ifcDateFromCalendarDate (Date.year date) SolIfc ((ordinalDay date) - 169)
    else if ordinalDay date < 226 then
      ifcDateFromCalendarDate (Date.year date) JulIfc ((ordinalDay date) - 197)
    else if ordinalDay date < 254 then
      ifcDateFromCalendarDate (Date.year date) AugIfc ((ordinalDay date) - 225)
    else if ordinalDay date < 282 then
      ifcDateFromCalendarDate (Date.year date) SepIfc ((ordinalDay date) - 253)
    else if ordinalDay date < 310 then
      ifcDateFromCalendarDate (Date.year date) OctIfc ((ordinalDay date) - 281)
    else if ordinalDay date < 338 then
      ifcDateFromCalendarDate (Date.year date) NovIfc ((ordinalDay date) - 309)
    else
      ifcDateFromCalendarDate (Date.year date) DecIfc ((ordinalDay date) - 337)
  else
    if ordinalDay date < 197 then
      ifcDateFromCalendarDate (Date.year date) SolIfc ((ordinalDay date) - 168)
    else if ordinalDay date < 225 then
      ifcDateFromCalendarDate (Date.year date) JulIfc ((ordinalDay date) - 196)
    else if ordinalDay date < 253 then
      ifcDateFromCalendarDate (Date.year date) AugIfc ((ordinalDay date) - 224)
    else if ordinalDay date < 281 then
      ifcDateFromCalendarDate (Date.year date) SepIfc ((ordinalDay date) - 252)
    else if ordinalDay date < 309 then
      ifcDateFromCalendarDate (Date.year date) OctIfc ((ordinalDay date) - 280)
    else if ordinalDay date < 337 then
      ifcDateFromCalendarDate (Date.year date) NovIfc ((ordinalDay date) - 308)
    else
      ifcDateFromCalendarDate (Date.year date) DecIfc ((ordinalDay date) - 336)

gregorianDateToString : Date -> String
gregorianDateToString date =
  (weekdayToString (Date.weekday date)) ++ " " ++
  (String.fromInt (Date.day date)) ++ " " ++
  (gregorianMonthToString (Date.month date)) ++ " " ++
  (String.fromInt (Date.year date))

ifcDateToWeekdayString : IfcDate -> String
ifcDateToWeekdayString date =
  if date.day == 29 && date.month == JunIfc then
    "Leap Day"
  else if date.day == 29 && date.month == DecIfc then
    "Year Day"
  else if modBy 7 date.day == 1 then
    "Sunday"
  else if modBy 7 date.day == 2 then
    "Monday"
  else if modBy 7 date.day == 3 then
    "Tuesday"
  else if modBy 7 date.day == 4 then
    "Wednesday"
  else if modBy 7 date.day == 5 then
    "Thursday"
  else if modBy 7 date.day == 6 then
    "Friday"
  else
    "Saturday"

ifcDateToString : IfcDate -> String
ifcDateToString date =
  if date.day == 29 then
    (ifcDateToWeekdayString date) ++ " " ++
    (String.fromInt date.year)
  else
    (ifcDateToWeekdayString date) ++ " " ++
    (String.fromInt date.day) ++ " " ++
    (ifcMonthToString date.month) ++ " " ++
    (String.fromInt date.year)

maybeDateToHtml : Maybe Date -> List (Html Msg)
maybeDateToHtml maybeDate = case maybeDate of
  Nothing -> [ text "Please pick a date." ]
  Just date ->
    [ text ((gregorianDateToString date) ++ " on the ")
    , strong [] [ text "Gregorian Calendar" ]
    , br [] []
    , text "corresponds to"
    , br [] []
    , text (
        (ifcDateToString (gregorianDateToIfcDate date)) ++ 
        " on the "
        )
    , strong [] [ text "International Fixed Calendar" ]
    , text "."
    ]

view : Model -> Html Msg
view model =
  div [] [ label [ (for "datepicker") ] [ (text "Choose a Gregorian Calendar date:") ]
    , DatePicker.view
      model.date
      someSettings
      model.datePicker
    |> Html.map SetDatePicker
    , p [] (maybeDateToHtml model.date)
    ]
