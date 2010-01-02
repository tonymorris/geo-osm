-- | Values with a @member@ accessor that is a list of @Member@.
module Data.Geo.OSM.Accessor.Members where

import Data.Geo.OSM.Member

class Members a where
  members :: a -> [Member]
  setMembers :: [Member] -> a -> a
