-- | Values with a @bounds@ accessor which is either empty, a @Bound@ or a @Bounds@.
module Geo.OSM.Accessor.BoundOrs where

import Geo.OSM.Bound
import Geo.OSM.Bounds

class BoundOrs a where
  boundOrs :: a -> x -> (Bound -> x) -> (Bounds -> x) -> x
