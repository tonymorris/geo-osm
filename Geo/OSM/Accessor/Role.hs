-- | Values with a @role@ string accessor.
module Geo.OSM.Accessor.Role where

class Role a where
  role :: a -> String
