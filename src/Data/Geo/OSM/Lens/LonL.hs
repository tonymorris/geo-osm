-- | Values with a @lon@ string accessor.
module Data.Geo.OSM.Lens.LonL where

import Data.Lens.Common

class LonL a where
  lonL ::
    Lens a String

