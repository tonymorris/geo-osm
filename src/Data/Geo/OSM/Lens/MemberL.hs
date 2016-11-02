-- | Values with a @member@ accessor that is a list of @Member@.
module Data.Geo.OSM.Lens.MemberL where

import Data.Geo.OSM.Member
import Control.Lens.Lens

class MemberL a where
  memberL ::
    Lens' a [Member]

