-- | Values with a @member@ accessor that is a list of @Member@.
module Data.Geo.OSM.Accessor.Members where

import Data.Geo.OSM.Member
import Data.Geo.OSM.Accessor.Accessor

class Members a where
  members :: a -> [Member]
  setMembers :: [Member] -> a -> a

  setMember :: Member -> a -> a
  setMember = setMembers . return

  usingMembers :: ([Member] -> [Member]) -> a -> a
  usingMembers = members `using` setMembers

  usingMember :: (Member -> Member) -> a -> a
  usingMember = usingMembers . map
