-- | Values with a @pending@ boolean accessor.
module Data.Geo.OSM.Lens.PendingL where

import Control.Lens.Lens

class PendingL a where
  pendingL ::
    Lens' a Bool

