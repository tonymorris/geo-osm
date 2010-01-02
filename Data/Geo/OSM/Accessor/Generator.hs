-- | Values with a @generator@ optional string accessor.
module Data.Geo.OSM.Accessor.Generator where

class Generator a where
  generator :: a -> Maybe String
  setGenerator :: Maybe String -> a -> a