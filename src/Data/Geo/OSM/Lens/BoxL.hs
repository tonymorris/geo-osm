-- | Values with a @box@ string accessor.
module Data.Geo.OSM.Lens.BoxL where

import Control.Lens.Lens

class BoxL a where
  boxL ::
    Lens' a String

