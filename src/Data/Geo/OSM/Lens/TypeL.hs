-- | Values with a @type@ accessor.
module Data.Geo.OSM.Lens.TypeL where

import Data.Geo.OSM.MemberType
import Data.Lens.Common

class TypeL a where
  typeL ::
    Lens a MemberType

