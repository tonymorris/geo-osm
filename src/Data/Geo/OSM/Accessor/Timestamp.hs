{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

-- | Values with a @timestamp@ optional string accessor.
module Data.Geo.OSM.Accessor.Timestamp where

import Data.Geo.OSM.Accessor.Accessor

class Timestamp a b | a -> b where
  timestamp :: a -> b
  setTimestamp :: b -> a -> a

  usingTimestamp :: (b -> b) -> a -> a
  usingTimestamp = timestamp `using` setTimestamp
