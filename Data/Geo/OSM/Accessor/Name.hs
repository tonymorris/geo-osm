-- -- | Values with a @name@ string accessor.
module Data.Geo.OSM.Accessor.Name where

import Data.Geo.OSM.Accessor.Accessor

class Name a where
  name :: a -> String
  setName :: String -> a -> a

  usingName :: (String -> String) -> a -> a
  usingName = name `using` setName
