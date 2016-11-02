-- -- | Values with a @maxlon@ string accessor.
module Data.Geo.OSM.Lens.MaxlonL where

import Control.Lens.Lens

class MaxlonL a where
  maxlonL ::
    Lens' a String

