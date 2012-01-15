-- | Values with a @zoom@ string accessor.
module Data.Geo.OSM.Lens.ZoomL where

import Data.Lens.Common

class ZoomL a where
  zoomL ::
    Lens a String
