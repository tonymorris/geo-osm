-- | Values with a @home@ optional string accessor.
module Data.Geo.OSM.Lens.HomeL where

import Data.Lens.Common
import Data.Geo.OSM.Home

class HomeL a where
  homeL ::
    Lens a (Maybe Home)

