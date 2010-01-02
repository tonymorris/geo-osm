-- | Values with a @v@ string accessor.
module Data.Geo.OSM.Accessor.V where

import Data.Geo.OSM.Accessor.Accessor

class V a where
  v :: a -> String
  setV :: String -> a -> a

  usingV :: a -> (String -> String) -> a
  usingV = v `using` setV
