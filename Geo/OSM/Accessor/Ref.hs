-- | Values with a @ref@ string accessor.
module Geo.OSM.Accessor.Ref where

class Ref a where
  ref :: a -> String
