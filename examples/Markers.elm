module Markers exposing (attractions, init, main, sydney)

import Browser
import Html
import Maps
import Maps.Geo
import Maps.Map as Map
import Maps.Marker as Marker


main =
    Browser.element
        { init = \() -> init
        , update = Maps.update
        , subscriptions = Maps.subscriptions
        , view = Maps.view
        }


init =
    ( Maps.defaultModel
        |> Maps.updateMap (Map.setZoom 14 >> Map.moveTo sydney)
        |> Maps.updateMarkers ((::) (Marker.createCustom (Html.text "Sydney") sydney))
        |> Maps.updateMarkers (\markers -> List.map Marker.create attractions ++ markers)
    , Cmd.none
    )


sydney =
    Maps.Geo.latLng -33.865143 151.2099


attractions =
    List.map (\( lat, lng ) -> Maps.Geo.latLng lat lng)
        [ ( -33.852324, 151.210819 )
        , ( -33.856872, 151.215239 )
        , ( -33.870397, 151.208835 )
        ]
