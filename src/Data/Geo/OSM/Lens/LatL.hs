-- | Values with a @lat@ string accessor.
module Data.Geo.OSM.Lens.LatL where

import Data.Lens.Common

class LatL a where
  latL ::
    Lens a String

