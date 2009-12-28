-- -- | Values with a @maxlon@ string accessor.
module Data.Geo.OSM.Accessor.Maxlon where

class Maxlon a where
  maxlon :: a -> String
