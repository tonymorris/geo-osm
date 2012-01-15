-- | Values with a @pending@ boolean accessor.
module Data.Geo.OSM.Lens.PendingL where

import Data.Lens.Common

class PendingL a where
  pendingL ::
    Lens a Bool

