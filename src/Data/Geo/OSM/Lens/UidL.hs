-- | Values with a @uid@ optional string accessor.
module Data.Geo.OSM.Lens.UidL where

import Data.Lens.Common

class UidL a where
  uidL ::
    Lens a (Maybe String)

