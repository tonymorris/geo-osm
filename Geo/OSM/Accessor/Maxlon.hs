-- -- | Values with a @maxlon@ string accessor.
module Geo.OSM.Accessor.Maxlon where

class Maxlon a where
  maxlon :: a -> String
