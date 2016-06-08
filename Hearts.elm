import Array exposing (fromList, get, length)
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick, onInput)
import Html.App as Html
import Platform.Cmd
import Random
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, millisecond)

main : Program Never
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type alias Heart =
  { x : Float
  , y : Float
  , color: String
  }

type alias Model =
  { time : Time
  , url: String
  , hearts : List Heart
  }

init : (Model, Cmd Msg)
init =
  ( { time = 0, url = "http://i.imgur.com/aNJGGKY.gif", hearts = [] }
  , Cmd.none
  )

type Msg
  = Tick Time
  | CreateHeart Int
  | RollForX
  | UrlChanged String

type MyCmd msg = Generate

update : Msg -> Model -> (Model, Cmd Msg)
update action model =
  case action of
    Tick newTime ->
      ({ model | time = newTime }, Cmd.none)
    RollForX ->
      (model, Random.generate CreateHeart (Random.int 1 80))
    UrlChanged url ->
      ({ model | url = url }, Cmd.none)
    CreateHeart coordinate ->
      let
        ms = Time.inMilliseconds model.time
        colors = Array.fromList
          [ "#FFFFCC", "#FFFF99", "#FFFF66", "#FFFF33", "#FFFF00", "#CCCC00"
          , "#FFCC66", "#FFCC00", "#FFCC33", "#CC9900", "#CC9933", "#996600"
          , "#FF9900", "#FF9933", "#CC9966", "#CC6600", "#996633", "#663300"
          , "#FFCC99", "#FF9966", "#FF6600", "#CC6633", "#993300", "#660000"
          , "#FF6633", "#CC3300", "#FF3300", "#FF0000", "#CC0000", "#990000"
          , "#FFCCCC", "#FF9999", "#FF6666", "#FF3333", "#FF0033", "#CC0033"
          , "#CC9999", "#CC6666", "#CC3333", "#993333", "#990033", "#330000"
          , "#FF6699", "#FF3366", "#FF0066", "#CC3366", "#996666", "#663333"
          , "#FF99CC", "#FF3399", "#FF0099", "#CC0066", "#993366", "#660033"
          , "#FF66CC", "#FF00CC", "#FF33CC", "#CC6699", "#CC0099", "#990066"
          , "#FFCCFF", "#FF99FF", "#FF66FF", "#FF33FF", "#FF00FF", "#CC3399"
          , "#CC99CC", "#CC66CC", "#CC00CC", "#CC33CC", "#990099", "#993399"
          , "#CC66FF", "#CC33FF", "#CC00FF", "#9900CC", "#996699", "#660066"
          , "#CC99FF", "#9933CC", "#9933FF", "#9900FF", "#660099", "#663366"
          , "#9966CC", "#9966FF", "#6600CC", "#6633CC", "#663399", "#330033"
          , "#CCCCFF", "#9999FF", "#6633FF", "#6600FF", "#330099", "#330066"
          , "#9999CC", "#6666FF", "#6666CC", "#666699", "#333399", "#333366"
          , "#3333FF", "#3300FF", "#3300CC", "#3333CC", "#000099", "#000066"
          , "#6699FF", "#3366FF", "#0000FF", "#0000CC", "#0033CC", "#000033"
          , "#0066FF", "#0066CC", "#3366CC", "#0033FF", "#003399", "#003366"
          , "#99CCFF", "#3399FF", "#0099FF", "#6699CC", "#336699", "#006699"
          , "#66CCFF", "#33CCFF", "#00CCFF", "#3399CC", "#0099CC", "#003333"
          , "#99CCCC", "#66CCCC", "#339999", "#669999", "#006666", "#336666"
          , "#CCFFFF", "#99FFFF", "#66FFFF", "#33FFFF", "#00FFFF", "#00CCCC"
          , "#99FFCC", "#66FFCC", "#33FFCC", "#00FFCC", "#33CCCC", "#009999"
          , "#66CC99", "#33CC99", "#00CC99", "#339966", "#009966", "#006633"
          , "#66FF99", "#33FF99", "#00FF99", "#33CC66", "#00CC66", "#009933"
          , "#99FF99", "#66FF66", "#33FF66", "#00FF66", "#339933", "#006600"
          , "#CCFFCC", "#99CC99", "#66CC66", "#669966", "#336633", "#003300"
          , "#33FF33", "#00FF33", "#00FF00", "#00CC00", "#33CC33", "#00CC33"
          , "#66FF00", "#66FF33", "#33FF00", "#33CC00", "#339900", "#009900"
          , "#CCFF99", "#99FF66", "#66CC00", "#66CC33", "#669933", "#336600"
          , "#99FF00", "#99FF33", "#99CC66", "#99CC00", "#99CC33", "#669900"
          ]
      in
        ({ model | hearts =
          { x = toFloat (10 + coordinate)
          , y = 100 + ms / 30
          , color =
            case (get (round ms % (length colors)) colors) of
              Just color -> color
              Nothing -> "black"
          } :: model.hearts }, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every (20 * millisecond) Tick

view : Model -> Html Msg
view model =
  let
    shakeX x =
      80 + 5 * sin (x + Time.inMilliseconds model.time / 200)
    shiftY y =
      round(y - (Time.inMilliseconds model.time) / 30) % 100
    angle x =
      -10 * sin (x + Time.inMilliseconds model.time / 200)
    toHeart h =
      let
        yValue =
          shiftY h.y
        opacityValue =
          (toString <| (toFloat yValue) / 100)
        fontSizeValue =
          if yValue < 90 then 16
          else (100 - yValue) + 6
      in
        text'
          [ x (toString <| shakeX h.x)
          , y (toString yValue)
          , fill h.color
          , fontSize <| toString fontSizeValue
          , stroke "white"
          , strokeWidth "0.5"
          , strokeOpacity opacityValue
          , fillOpacity opacityValue
          , transform <| "rotate(" ++ (toString <| angle h.x) ++ " " ++ (toString <| shakeX h.x) ++ " " ++ (toString yValue) ++ ")"
          ]
          [ Svg.text "❤" ]
    hearts =
      List.map toHeart model.hearts
  in
    div []
      [ h2 [] [ Html.text "Click to heart!" ]
      , input
        [ Html.Attributes.style
          [ ("width", "500px")
          , ("font-family", "monospace")
          ]
        , Html.Attributes.value model.url
        , onInput UrlChanged
        ] []
      , div
        [ Html.Attributes.style
          [ ("cursor", "pointer")
          , ("background-image", "url(" ++ model.url ++ ")")
          , ("background-position", "center")
          , ("background-size", "auto 100%")
          , ("width", "500px")
          , ("height", "500px")
          , ("-webkit-user-select", "none")
          , ("-khtml-user-select", "none")
          , ("-moz-user-select", "none")
          ]
        , onClick RollForX
        ]
        [ Svg.svg [ viewBox "0 0 100 100", width "500px", height "500px", fill "none" ]
          (hearts)
        ]
      ]
