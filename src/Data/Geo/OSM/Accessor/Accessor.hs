module Data.Geo.OSM.Accessor.Accessor(
                                       using
                                     ) where

import Control.Monad.Instances()

using :: (a -> d) -> (c -> a -> b) -> (d -> c) -> a -> b
using f g = flip (flip =<< (g .) . flip id . f)
