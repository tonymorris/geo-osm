-- | Values with a @k@ string accessor.
module Data.Geo.OSM.Accessor.K where

class K a where
  k :: a -> String
  setK :: String -> a -> a
