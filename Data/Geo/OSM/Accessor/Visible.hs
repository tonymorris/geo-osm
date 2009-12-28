-- | Values with a @visible@ boolean accessor.
module Data.Geo.OSM.Accessor.Visible where

class Visible a where
  visible :: a -> Bool
