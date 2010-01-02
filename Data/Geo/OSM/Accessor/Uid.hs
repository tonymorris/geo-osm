-- | Values with a @uid@ optional string accessor.
module Data.Geo.OSM.Accessor.Uid where

class Uid a where
  uid :: a -> Maybe String
  setUid :: Maybe String -> a -> a
