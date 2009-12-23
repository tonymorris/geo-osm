-- | Values with a @user@ optional string accessor.
module Geo.OSM.Accessor.User where

class User a where
  user :: a -> Maybe String
