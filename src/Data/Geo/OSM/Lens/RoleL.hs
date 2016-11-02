-- | Values with a @role@ string accessor.
module Data.Geo.OSM.Lens.RoleL where

import Control.Lens.Lens

class RoleL a where
  roleL ::
    Lens' a String

