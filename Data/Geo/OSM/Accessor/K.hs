-- | Values with a @k@ string accessor.
module Data.Geo.OSM.Accessor.K where

import Data.Geo.OSM.Accessor.Accessor

class K a where
  k :: a -> String
  setK :: String -> a -> a

  updateK :: a -> (String -> String) -> a
  updateK = k `using` setK
