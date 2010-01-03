-- | Values with a @uid@ optional string accessor.
module Data.Geo.OSM.Accessor.Uid where

import Data.Geo.OSM.Accessor.Accessor

class Uid a where
  uid :: a -> Maybe String
  setUid :: Maybe String -> a -> a

  setUid' :: String -> a -> a
  setUid' = setUid . return

  usingUid :: a -> (Maybe String -> Maybe String) -> a
  usingUid = uid `using` setUid

  usingUid' :: a -> (String -> String) -> a
  usingUid' = (. fmap) . usingUid
