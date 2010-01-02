-- | Values with a @lat@ string accessor.
module Data.Geo.OSM.Accessor.Lat where

import Data.Geo.OSM.Accessor.Accessor

class Lat a where
  lat :: a -> String
  setLat :: String -> a -> a

  usingLat :: a -> (String -> String) -> a
  usingLat = lat `using` setLat
