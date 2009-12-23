-- | Values with a @timestamp@ optional string accessor.
module Geo.OSM.Accessor.Timestamp where

class Timestamp a where
  timestamp :: a -> Maybe String
