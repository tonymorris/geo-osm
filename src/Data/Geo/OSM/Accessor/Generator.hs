-- | Values with a @generator@ optional string accessor.
module Data.Geo.OSM.Accessor.Generator where

import Data.Geo.OSM.Accessor.Accessor

class Generator a where
  generator :: a -> Maybe String
  setGenerator :: Maybe String -> a -> a

  setGenerator' :: String -> a -> a
  setGenerator' = setGenerator . return

  usingGenerator :: (Maybe String -> Maybe String) -> a -> a
  usingGenerator = generator `using` setGenerator

  usingGenerator' :: (String -> String) -> a -> a
  usingGenerator' = usingGenerator . fmap
