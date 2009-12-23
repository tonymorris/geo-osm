-- | Values with a @minlon@ string accessor.
module Geo.OSM.Accessor.Minlon where

class Minlon a where
  minlon :: a -> String
