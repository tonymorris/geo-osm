-- | Values with a @home@ optional string accessor.
module Data.Geo.OSM.Lens.HomeL where

import Control.Lens.Lens
import Data.Geo.OSM.Home

class HomeL a where
  homeL ::
    Lens' a (Maybe Home)

