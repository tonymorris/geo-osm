-- | Values with a @maxlat@ string accessor.
module Data.Geo.OSM.Lens.MaxlatL where

import Control.Lens.Lens

class MaxlatL a where
  maxlatL :: 
    Lens' a String

