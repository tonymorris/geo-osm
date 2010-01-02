-- | Values with a @zoom@ string accessor.
module Data.Geo.OSM.Accessor.Zoom where

import Data.Geo.OSM.Accessor.Accessor

class Zoom a where
  zoom :: a -> String
  setZoom :: String -> a -> a

  usingZoom :: a -> (String -> String) -> a
  usingZoom = zoom  `using` setZoom
