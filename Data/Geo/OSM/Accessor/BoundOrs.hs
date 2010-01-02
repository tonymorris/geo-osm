-- | Values with a @bounds@ accessor which is either empty, a @Bound@ or a @Bounds@.
module Data.Geo.OSM.Accessor.BoundOrs where

import Data.Geo.OSM.Bound
import Data.Geo.OSM.Bounds

class BoundOrs a where
  boundOrs :: a -> x -> (Bound -> x) -> (Bounds -> x) -> x
  setBoundOrs :: Maybe (Either Bound Bounds) -> a -> a
