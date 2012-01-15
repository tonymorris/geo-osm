-- -- | Values with a @maxlon@ string accessor.
module Data.Geo.OSM.Lens.MaxlonL where

import Data.Lens.Common

class MaxlonL a where
  maxlonL ::
    Lens a String

