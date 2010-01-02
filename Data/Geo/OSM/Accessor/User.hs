-- | Values with a @user@ optional string accessor.
module Data.Geo.OSM.Accessor.User where

import Data.Geo.OSM.Accessor.Accessor

class User a where
  user :: a -> Maybe String
  setUser :: Maybe String -> a -> a

  setUser' :: String -> a -> a
  setUser' = setUser . return

  usingUser :: a -> (Maybe String -> Maybe String) -> a
  usingUser = user `using` setUser
