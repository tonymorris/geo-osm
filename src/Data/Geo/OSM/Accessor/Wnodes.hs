-- | Values with a @waynodes@ string accessor.
module Data.Geo.OSM.Accessor.Wnodes where

import Data.Geo.OSM.Waynodes
import Data.Geo.OSM.Accessor.Accessor

class Wnodes a where
  wnodes :: a -> Waynodes
  setWnodes :: Waynodes -> a -> a

  usingWnodes :: (Waynodes -> Waynodes) -> a -> a
  usingWnodes = wnodes `using` setWnodes
