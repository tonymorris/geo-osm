-- | Values with a @nd@ accessor that is a list of @Nd@.
module Data.Geo.OSM.Accessor.Nds where

import Data.Geo.OSM.Nd

class Nds a where
  nds :: a -> [Nd]
  setNds :: [Nd] -> a -> a

  setNd :: Nd -> a -> a
  setNd = setNds . return
