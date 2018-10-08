module Maps.Internal.Zoom exposing
  ( EventOptions
  , fromPinch
  , events
  )

import Json.Decode as Json

import Html
import Html.Events exposing (on)

import Maps.Internal.Screen as Screen exposing (ZoomLevel)
import Maps.Internal.Pinch as Pinch exposing (Pinch)

type alias EventOptions msg =
  { zoom : Screen.Offset -> ZoomLevel -> msg
  , pinchStart : Screen.TwoFingers -> msg
  , pinchTo : Screen.TwoFingers -> msg
  , pinchStop : msg
  }

fromPinch : Float -> Float -> Pinch -> (ZoomLevel, Screen.Offset)
fromPinch mapWidth mapHeight pinch =
  let
    (start, end) = Pinch.startEnd pinch
  in
    ( logBase 2 (end.length / start.length)
    , start.center
    )

events : EventOptions msg -> ZoomLevel -> List (Html.Attribute msg)
events ({zoom, pinchStart, pinchTo, pinchStop}) mapZoom =
  [ -- Mouse
    Html.Events.custom "dblclick"
    <| Json.map (\v -> { message = v, preventDefault = True, stopPropagation = False })
    <| Json.map (\offset -> zoom offset 1)
    <| Screen.decodeOffset
  , --Mouse
    Html.Events.custom "wheel"
    <| Json.map (\v -> { message = v, preventDefault = True, stopPropagation = False })
      <| Json.map2
        zoom
        Screen.decodeOffset
        Screen.decodeZoom
  , -- Mobile
    Html.Events.custom "touchstart"
    <| Json.map (\v -> { message = v, preventDefault = True, stopPropagation = False })
    <| Json.map (Maybe.withDefault pinchStop)
    <| Json.map (Maybe.map pinchStart)
    <| Screen.decodeTwoFingers
  , -- Mobile
    Html.Events.custom "touchmove"
    <| Json.map (\v -> { message = v, preventDefault = True, stopPropagation = False })
    <| Json.map (Maybe.withDefault pinchStop)
    <| Json.map (Maybe.map pinchTo)
    <| Screen.decodeTwoFingers
  , -- Mobile
    Html.Events.custom "touchend"
    <| Json.map (\v -> { message = v, preventDefault = True, stopPropagation = False })
    <| Json.succeed pinchStop
  ]
