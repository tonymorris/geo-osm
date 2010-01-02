-- | Values with a @tracepoints@ string accessor.
module Data.Geo.OSM.Accessor.Tpoints where

import Data.Geo.OSM.Tracepoints
import Data.Geo.OSM.Accessor.Accessor

class Tpoints a where
  tpoints :: a -> Tracepoints
  setTpoints :: Tracepoints -> a -> a

  usingTpoints :: a -> (Tracepoints -> Tracepoints) -> a
  usingTpoints = tpoints `using` setTpoints
