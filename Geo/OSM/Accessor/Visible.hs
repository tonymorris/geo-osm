-- | Values with a @visible@ boolean accessor.
module Geo.OSM.Accessor.Visible where

class Visible a where
  visible :: a -> Bool
