-- | Values with a @v@ string accessor.
module Data.Geo.OSM.Lens.VL where

import Control.Lens.Lens

class VL a where
  vL ::
    Lens' a String

