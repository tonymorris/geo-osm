-- | Values with a @area@ string accessor.
module Data.Geo.OSM.Accessor.Ar where

import Data.Geo.OSM.Area
import Data.Geo.OSM.Accessor.Accessor

class Ar a where
  ar :: a -> Area
  setAr :: Area -> a -> a

  usingArea :: (Area -> Area) -> a -> a
  usingArea = ar `using` setAr
