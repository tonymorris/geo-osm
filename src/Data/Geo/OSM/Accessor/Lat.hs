-- | Values with a @lat@ string accessor.
module Data.Geo.OSM.Accessor.Lat where

import Data.Geo.OSM.Accessor.Accessor

class Lat a where
  lat :: a -> String
  setLat :: String -> a -> a

  usingLat :: (String -> String) -> a -> a
  usingLat = lat `using` setLat
