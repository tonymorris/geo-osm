-- | Values with a @origin@ optional string accessor.
module Data.Geo.OSM.Accessor.Origin where

import Data.Geo.OSM.Accessor.Accessor

class Origin a where
  origin :: a -> Maybe String
  setOrigin :: Maybe String -> a -> a

  setOrigin' :: String -> a -> a
  setOrigin' = setOrigin . return

  usingOrigin :: a -> (Maybe String -> Maybe String) -> a
  usingOrigin = origin `using` setOrigin
