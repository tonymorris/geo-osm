-- | Values with a @ref@ string accessor.
module Data.Geo.OSM.Accessor.Ref where

import Data.Geo.OSM.Accessor.Accessor

class Ref a where
  ref :: a -> String
  setRef :: String -> a -> a

  usingRef :: (String -> String) -> a -> a
  usingRef = ref `using` setRef
