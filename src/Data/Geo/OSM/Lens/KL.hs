-- | Values with a @k@ string accessor.
module Data.Geo.OSM.Lens.KL where

import Control.Lens.Lens

class KL a where
  kL ::
    Lens' a String

