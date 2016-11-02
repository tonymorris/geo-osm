-- | Values with a @lon@ string accessor.
module Data.Geo.OSM.Lens.LonL where

import Control.Lens.Lens

class LonL a where
  lonL ::
    Lens' a String

