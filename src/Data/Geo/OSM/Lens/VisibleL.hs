-- | Values with a @visible@ boolean accessor.
module Data.Geo.OSM.Lens.VisibleL where

import Control.Lens.Lens

class VisibleL a where
  visibleL ::
    Lens' a Bool

