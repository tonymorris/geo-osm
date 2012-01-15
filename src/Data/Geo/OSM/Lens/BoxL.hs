-- | Values with a @box@ string accessor.
module Data.Geo.OSM.Lens.BoxL where

import Data.Lens.Common

class BoxL a where
  boxL ::
    Lens a String

