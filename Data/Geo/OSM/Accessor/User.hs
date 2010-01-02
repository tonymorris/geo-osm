-- | Values with a @user@ optional string accessor.
module Data.Geo.OSM.Accessor.User where

class User a where
  user :: a -> Maybe String
  setUser :: Maybe String -> a -> a

  setUser' :: String -> a -> a
  setUser' = setUser . return
