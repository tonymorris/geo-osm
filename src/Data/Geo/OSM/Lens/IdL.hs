-- | Values with a @id@ string accessor.
module Data.Geo.OSM.Lens.IdL where

import Control.Lens.Lens

class IdL a where
  idL ::
    Lens' a String

