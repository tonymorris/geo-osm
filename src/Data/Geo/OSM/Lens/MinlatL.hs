-- | Values with a @minlat@ string accessor.
module Data.Geo.OSM.Lens.MinlatL where

import Control.Lens.Lens

class MinlatL a where
  minlatL ::
    Lens' a String

