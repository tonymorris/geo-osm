-- | Values with a @v@ string accessor.
module Geo.OSM.Accessor.V where

class V a where
  v :: a -> String
