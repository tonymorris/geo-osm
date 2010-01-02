-- -- | Values with a @maxlon@ string accessor.
module Data.Geo.OSM.Accessor.Maxlon where

import Data.Geo.OSM.Accessor.Accessor

class Maxlon a where
  maxlon :: a -> String
  setMaxlon :: String -> a -> a

  usingMaxlon :: a -> (String -> String) -> a
  usingMaxlon = maxlon `using` setMaxlon
