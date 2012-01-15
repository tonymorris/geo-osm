-- | Values with a @maxlat@ string accessor.
module Data.Geo.OSM.Accessor.Maxlat where

import Data.Geo.OSM.Accessor.Accessor

class Maxlat a where
  maxlat :: a -> String
  setMaxlat :: String -> a -> a

  usingMaxlat :: (String -> String) -> a -> a
  usingMaxlat = maxlat  `using` setMaxlat
