-- | Values with a @minlat@ string accessor.
module Data.Geo.OSM.Accessor.Minlat where

class Minlat a where
  minlat :: a -> String
