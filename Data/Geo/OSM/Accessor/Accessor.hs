module Data.Geo.OSM.Accessor.Accessor(
                                       using
                                     ) where

using :: (a -> d) -> (c -> a -> b) -> a -> (d -> c) -> b
using f g = flip =<< (g .) . flip id . f
