-- | Values with a @timestamp@ optional string accessor.
module Data.Geo.OSM.Accessor.Timestamp where

import Data.Geo.OSM.Accessor.Accessor

class Timestamp a where
  timestamp :: a -> Maybe String
  setTimestamp :: Maybe String -> a -> a

  setTimestamp' :: String -> a -> a
  setTimestamp' = setTimestamp . return

  usingTimestamp :: a -> (Maybe String -> Maybe String) -> a
  usingTimestamp = timestamp `using` setTimestamp
