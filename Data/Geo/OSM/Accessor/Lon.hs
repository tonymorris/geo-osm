-- | Values with a @lon@ string accessor.
module Data.Geo.OSM.Accessor.Lon where

import Data.Geo.OSM.Accessor.Accessor

class Lon a where
  lon :: a -> String
  setLon :: String -> a -> a

  usingLon :: (String -> String) -> a -> a
  usingLon = lon `using` setLon
