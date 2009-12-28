-- | Values with a @version@ string accessor.
module Data.Geo.OSM.Accessor.Version where

class Version a where
  version :: a -> String
