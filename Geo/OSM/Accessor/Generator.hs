-- | Values with a @generator@ optional string accessor.
module Geo.OSM.Accessor.Generator where

class Generator a where
  generator :: a -> Maybe String
