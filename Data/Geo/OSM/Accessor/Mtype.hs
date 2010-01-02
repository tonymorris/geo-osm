-- | Values with a @type@ accessor.
module Data.Geo.OSM.Accessor.Mtype where

import Data.Geo.OSM.MemberType

class Mtype a where
  mtype :: a -> MemberType
  setMtype :: MemberType -> a -> a
