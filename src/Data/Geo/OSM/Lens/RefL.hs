-- | Values with a @ref@ string accessor.
module Data.Geo.OSM.Lens.RefL where

import Control.Lens.Lens

class RefL a where
  refL ::
    Lens' a String

