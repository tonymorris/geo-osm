-- | Values with a @maxlat@ string accessor.
module Data.Geo.OSM.Accessor.Maxlat where

class Maxlat a where
  maxlat :: a -> String
