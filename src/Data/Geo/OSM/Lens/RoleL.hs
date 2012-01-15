-- | Values with a @role@ string accessor.
module Data.Geo.OSM.Lens.RoleL where

import Data.Lens.Common

class RoleL a where
  roleL ::
    Lens a String

