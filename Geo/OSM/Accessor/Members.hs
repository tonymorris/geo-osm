-- | Values with a @member@ accessor that is a list of @Member@.
module Geo.OSM.Accessor.Members where

import Geo.OSM.Member

class Members a where
  members :: a -> [Member]
