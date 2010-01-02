-- | Values with a @origin@ optional string accessor.
module Data.Geo.OSM.Accessor.Origin where

class Origin a where
  origin :: a -> Maybe String
  setOrigin :: Maybe String -> a -> a
