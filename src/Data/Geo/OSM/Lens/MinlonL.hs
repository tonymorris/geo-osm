-- | Values with a @minlon@ string accessor.
module Data.Geo.OSM.Lens.MinlonL where

import Data.Lens.Common

class MinlonL a where
  minlonL ::
    Lens a String

