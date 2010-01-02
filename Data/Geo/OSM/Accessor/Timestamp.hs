-- | Values with a @timestamp@ optional string accessor.
module Data.Geo.OSM.Accessor.Timestamp where

class Timestamp a where
  timestamp :: a -> Maybe String
  setTimestamp :: Maybe String -> a -> a

  setTimestamp' :: String -> a -> a
  setTimestamp' = setTimestamp . return
