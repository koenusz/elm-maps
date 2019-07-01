module Maps.Marker exposing
    ( Marker
    , create
    , createCustom
    , mapTolatLng
    )

{-| Markers are for displaying geographic locations on the map.

@docs Marker


# Create a marker

@docs create
@docs createCustom

-}

import Html exposing (Html)
import Maps.Geo
import Maps.Internal.Marker as Marker exposing (Marker(..))


{-| There are currently two types of marker:

  - A default marker
  - A custom HTML marker

-}
type alias Marker msg =
    Marker.Marker msg


{-| Create a default style of marker at the given latitude/longitude.

    import Maps.Geo
    import Maps.Marker

    newYork =
        Maps.Geo.latLng 40.73061 -73.935242

    newYorkMarker =
        Maps.Marker.create newYork

-}
create : Maps.Geo.LatLng -> Marker msg
create =
    Marker.DefaultMarker


{-| Create a custom HTML marker at the given latitude/longitude.
-}
createCustom : Html msg -> Maps.Geo.LatLng -> Marker msg
createCustom =
    Marker.CustomMarker

{-| Maps a marker to the lattitude/lonngitude .
-}
mapTolatLng: Marker msg -> Maps.Geo.LatLng
mapTolatLng = 
    Marker.mapTolatLng 

