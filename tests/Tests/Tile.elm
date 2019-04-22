module Tests.Tile exposing (conversionsTest)

import Expects exposing (..)
import Fuzzers exposing (..)
import Maps.Internal.LatLng exposing (LatLng)
import Maps.Internal.Tile exposing (..)
import Test exposing (..)


conversionsTest : Test
conversionsTest =
    describe "Tile offset conversion tests"
        [ fuzz latLng "Converting a LatLng to and from a tile offset should cause no change in the LatLng" <|
            \loc ->
                let
                    zoom =
                        15

                    tile =
                        fromLatLng zoom loc

                    convertedLoc =
                        toLatLng zoom tile.x tile.y
                in
                equalLatLng 0.0001
                    loc
                    convertedLoc
        ]
