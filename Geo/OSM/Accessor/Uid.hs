-- | Values with a @uid@ optional string accessor.
module Geo.OSM.Accessor.Uid where

class Uid a where
  uid :: a -> Maybe String
