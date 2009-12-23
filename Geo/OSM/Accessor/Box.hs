-- | Values with a @box@ string accessor.
module Geo.OSM.Accessor.Box where

class Box a where
  box :: a -> String
