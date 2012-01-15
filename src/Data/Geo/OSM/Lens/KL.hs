-- | Values with a @k@ string accessor.
module Data.Geo.OSM.Lens.KL where

import Data.Lens.Common

class KL a where
  kL ::
    Lens a String

