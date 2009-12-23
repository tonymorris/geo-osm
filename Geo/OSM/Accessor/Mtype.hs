-- | Values with a @type@ accessor.
module Geo.OSM.Accessor.Mtype where

import Geo.OSM.MemberType

class Mtype a where
  mtype :: a -> MemberType
