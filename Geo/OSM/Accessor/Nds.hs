-- | Values with a @nd@ accessor that is a list of @Nd@.
module Geo.OSM.Accessor.Nds where

import Geo.OSM.Nd

class Nds a where
  nds :: a -> [Nd]
