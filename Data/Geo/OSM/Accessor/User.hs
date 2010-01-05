{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

-- | Values with a @user@ optional string accessor.
module Data.Geo.OSM.Accessor.User where

import Data.Geo.OSM.Accessor.Accessor

class User a b | a -> b where
  user :: a -> b
  setUser :: b -> a -> a

  usingUser :: (b -> b) -> a -> a
  usingUser = user `using` setUser
