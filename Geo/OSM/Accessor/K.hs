-- | Values with a @k@ string accessor.
module Geo.OSM.Accessor.K where

class K a where
  k :: a -> String
