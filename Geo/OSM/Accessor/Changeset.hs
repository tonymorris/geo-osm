-- | Values with a @changeset@ optional string accessor.
module Geo.OSM.Accessor.Changeset where

class Changeset a where
  changeset :: a -> Maybe String
