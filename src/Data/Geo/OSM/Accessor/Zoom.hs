-- | Values with a @zoom@ string accessor.
module Data.Geo.OSM.Accessor.Zoom where

import Data.Geo.OSM.Accessor.Accessor

class Zoom a where
  zoom :: a -> String
  setZoom :: String -> a -> a

  usingZoom :: (String -> String) -> a -> a
  usingZoom = zoom  `using` setZoom
