-- | Values with a @visible@ boolean accessor.
module Data.Geo.OSM.Lens.VisibleL where

import Data.Lens.Common

class VisibleL a where
  visibleL ::
    Lens a Bool

