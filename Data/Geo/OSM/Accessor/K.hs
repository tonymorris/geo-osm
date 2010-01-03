-- | Values with a @k@ string accessor.
module Data.Geo.OSM.Accessor.K where

import Data.Geo.OSM.Accessor.Accessor

class K a where
  k :: a -> String
  setK :: String -> a -> a

  usingK :: a -> (String -> String) -> a
  usingK = k `using` setK
