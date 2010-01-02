-- | Values with a @changeset@ optional string accessor.
module Data.Geo.OSM.Accessor.Changeset where

class Changeset a where
  changeset :: a -> Maybe String
  setChangeset :: Maybe String -> a -> a

  setChangeset' :: String -> a -> a
  setChangeset' = setChangeset . return
