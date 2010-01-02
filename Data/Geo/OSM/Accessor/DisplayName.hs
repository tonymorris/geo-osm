-- | Values with a @display_name@ string accessor.
module Data.Geo.OSM.Accessor.DisplayName where

import Data.Geo.OSM.Accessor.Accessor

class DisplayName a where
  displayName :: a -> String
  setDisplayName :: String -> a -> a

  usingDisplayName :: a -> (String -> String) -> a
  usingDisplayName = displayName `using` setDisplayName
