-- | Values with a @area@ string accessor.
module Data.Geo.OSM.Lens.AreaL where

import Data.Geo.OSM.Area
import Control.Lens.Lens

class AreaL a where
  areaL ::
    Lens' a Area
