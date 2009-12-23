-- | Values with a @origin@ optional string accessor.
module Geo.OSM.Accessor.Origin where

class Origin a where
  origin :: a -> Maybe String
