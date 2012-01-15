-- | Values with a @area@ string accessor.
module Data.Geo.OSM.Lens.AreaL where

import Data.Geo.OSM.Area
import Data.Lens.Common

class AreaL a where
  areaL ::
    Lens a Area
