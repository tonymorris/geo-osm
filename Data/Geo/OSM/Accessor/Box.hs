-- | Values with a @box@ string accessor.
module Data.Geo.OSM.Accessor.Box where

class Box a where
  box :: a -> String
  setBox :: String -> a -> a
