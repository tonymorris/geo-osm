-- | Values with a @ref@ string accessor.
module Data.Geo.OSM.Accessor.Ref where

class Ref a where
  ref :: a -> String
  setRef :: String -> a -> a
