-- | Values with a @minlat@ string accessor.
module Data.Geo.OSM.Lens.MinlatL where

import Data.Lens.Common

class MinlatL a where
  minlatL ::
    Lens a String

