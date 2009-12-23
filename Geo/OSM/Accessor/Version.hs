-- | Values with a @version@ string accessor.
module Geo.OSM.Accessor.Version where

class Version a where
  version :: a -> String
