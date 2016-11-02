-- | Values with a @bounds@ accessor which is either empty, a @Bound@ or a @Bounds@.
module Data.Geo.OSM.Lens.BoundsL where

import Control.Lens.Lens
import Data.Geo.OSM.BoundOption

class BoundsL a where
  boundsL ::
    Lens' a BoundOption

