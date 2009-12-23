-- | Values with a @minlat@ string accessor.
module Geo.OSM.Accessor.Minlat where

class Minlat a where
  minlat :: a -> String
