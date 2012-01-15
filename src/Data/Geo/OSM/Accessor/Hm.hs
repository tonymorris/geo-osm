-- | Values with a @home@ optional string accessor.
module Data.Geo.OSM.Accessor.Hm where

import Data.Geo.OSM.Accessor.Accessor
import Data.Geo.OSM.Home

class Hm a where
  hm :: a -> Maybe Home
  setHm :: Maybe Home -> a -> a

  setHm' :: Home -> a -> a
  setHm' = setHm . return

  usingHm :: (Maybe Home -> Maybe Home) -> a -> a
  usingHm = hm `using` setHm

  usingHm' :: (Home -> Home) -> a -> a
  usingHm' = usingHm . fmap
