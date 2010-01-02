-- | Values with a @minimum@ string accessor.
module Data.Geo.OSM.Accessor.Minimum where

import Data.Geo.OSM.Accessor.Accessor
import Prelude hiding (minimum)

class Minimum a where
  minimum :: a -> String
  setMinimum :: String -> a -> a

  usingMinimum :: a -> (String -> String) -> a
  usingMinimum = minimum  `using` setMinimum
