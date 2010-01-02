module Data.Geo.OSM.Accessor.Accessor(
                                       using
                                     ) where

import Control.Monad.Instances()

using :: (a -> d) -> (c -> a -> b) -> a -> (d -> c) -> b
using f g = flip =<< (g .) . flip id . f
