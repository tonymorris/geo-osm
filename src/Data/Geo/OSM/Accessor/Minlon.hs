-- | Values with a @minlon@ string accessor.
module Data.Geo.OSM.Accessor.Minlon where

import Data.Geo.OSM.Accessor.Accessor

class Minlon a where
  minlon :: a -> String
  setMinlon :: String -> a -> a

  usingMinlon :: (String -> String) -> a -> a
  usingMinlon = minlon `using` setMinlon