-- | Values with a @id@ string accessor.
module Data.Geo.OSM.Accessor.Id where

class Id a where
  id :: a -> String
  setId :: String -> a -> a
