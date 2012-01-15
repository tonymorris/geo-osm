-- | Values with a @waynodes@ string accessor.
module Data.Geo.OSM.Lens.WaynodesL where

import Data.Geo.OSM.Waynodes
import Data.Lens.Common

class WaynodesL a where
  waynodesL ::
    Lens a Waynodes

