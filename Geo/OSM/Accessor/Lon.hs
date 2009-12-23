-- | Values with a @lon@ string accessor.
module Geo.OSM.Accessor.Lon where

class Lon a where
  lon :: a -> String
