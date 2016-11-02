-- | Values with a @minlon@ string accessor.
module Data.Geo.OSM.Lens.MinlonL where

import Control.Lens.Lens

class MinlonL a where
  minlonL ::
    Lens' a String

