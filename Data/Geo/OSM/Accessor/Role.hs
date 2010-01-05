-- | Values with a @role@ string accessor.
module Data.Geo.OSM.Accessor.Role where

import Data.Geo.OSM.Accessor.Accessor

class Role a where
  role :: a -> String
  setRole :: String -> a -> a

  usingRole :: (String -> String) -> a -> a
  usingRole = role `using` setRole
