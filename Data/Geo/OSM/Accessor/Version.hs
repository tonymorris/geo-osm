-- | Values with a @version@ string accessor.
module Data.Geo.OSM.Accessor.Version where

import Data.Geo.OSM.Accessor.Accessor

class Version a where
  version :: a -> String
  setVersion :: String -> a -> a

  usingVersion :: a -> (String -> String) -> a
  usingVersion = version `using` setVersion
