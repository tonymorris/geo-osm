-- | Values with a @lat@ string accessor.
module Geo.OSM.Accessor.Lat where

class Lat a where
  lat :: a -> String
