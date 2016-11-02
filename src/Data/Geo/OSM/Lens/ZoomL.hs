-- | Values with a @zoom@ string accessor.
module Data.Geo.OSM.Lens.ZoomL where

import Control.Lens.Lens

class ZoomL a where
  zoomL ::
    Lens' a String
