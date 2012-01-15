-- | Values with a @v@ string accessor.
module Data.Geo.OSM.Lens.VL where

import Data.Lens.Common

class VL a where
  vL ::
    Lens a String

