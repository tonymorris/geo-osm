-- | Values with a @nd@ accessor that is a list of @Nd@.
module Data.Geo.OSM.Lens.NdL where

import Data.Geo.OSM.Nd
import Data.Lens.Common

class NdL a where
  ndL ::
    Lens a [Nd]

