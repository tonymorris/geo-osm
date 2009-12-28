-- | Values with a @lon@ string accessor.
module Data.Geo.OSM.Accessor.Lon where

class Lon a where
  lon :: a -> String
