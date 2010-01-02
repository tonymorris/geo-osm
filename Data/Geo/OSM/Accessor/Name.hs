-- -- | Values with a @name@ string accessor.
module Data.Geo.OSM.Accessor.Name where

import Data.Geo.OSM.Accessor.Accessor

class Name a where
  name :: a -> String
  setName :: String -> a -> a

  usingName :: a -> (String -> String) -> a
  usingName = name `using` setName
