-- | Values with a @id@ string accessor.
module Geo.OSM.Accessor.Id where

class Id a where
  id :: a -> String
