{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

-- | Values with a @version@ string accessor.
module Data.Geo.OSM.Accessor.Version where

import Data.Geo.OSM.Accessor.Accessor

class Version a b | a -> b where
  version :: a -> b
  setVersion :: b -> a -> a

  usingVersion :: (b -> b) -> a -> a
  usingVersion = version `using` setVersion
