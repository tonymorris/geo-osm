-- | Values with a @lat@ string accessor.
module Data.Geo.OSM.Lens.LatL where

import Control.Lens.Lens

class LatL a where
  latL ::
    Lens' a String

